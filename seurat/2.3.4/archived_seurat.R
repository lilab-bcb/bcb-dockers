#!/usr/bin/env Rscript

# Detach any thing for Seurat
if ('package:Seurat' %in% search()) {
  detach(name = 'package:Seurat')
}

if ('Seurat' %in% loadedNamespaces()) {
  unloadNamespace(ns = 'Seurat')
}

local(expr = {
  # Prep for future Seurat version argument
  # Get latest archived version from CRAN
  archive.meta <- gzcon(con = url(
    description = 'https://cran.r-project.org/src/contrib/Meta/archive.rds',
    open = 'rb'
  ))
  archive <- readRDS(file = archive.meta)[['Seurat']]
  close(con = archive.meta)
  default.version <- rownames(x = archive)[nrow(x = archive)]
  default.version <- basename(path = tools::file_path_sans_ext(
    x = default.version,
    compression = TRUE
  ))
  default.version <- unlist(x = strsplit(x = default.version, split = '_'))
  default.version <- default.version[length(x = default.version)]
  # Get which version we're installing here
  target.version <- getOption(x = 'Seurat.installer.version', default = default.version)
  target.version <- package_version(x = as.character(x = target.version))
  if (target.version != package_version(x = default.version)) {
    warning(
      "The 'Seurat.installer.version' option is not currently used",
      immediate. = TRUE,
      call. = FALSE
    )
  }
  # Ensure we aren't wasting our time
  repo <- 'https://satijalab.org/ran'
  remote.version <- package_version(x = utils::available.packages(repos = repo)['Seurat', 'Version'])
  installed.version <- tryCatch(
    expr = packageVersion(pkg = 'Seurat'),
    error = function(...) {
      return(NULL)
    }
  )
  if (!is.null(x = installed.version) && installed.version == remote.version) {
      message("Seurat v", as.character(x = remote.version), " already installed")
  } else {
    if (!is.null(x = installed.version) && installed.version >= remote.version) {
      warning(
        "Downgrading Seurat from v",
        as.character(x = installed.version),
        " to v",
        as.character(x = remote.version),
        immediate. = TRUE,
        call. = FALSE
      )
    }
    # This works since we only host Seurat 2.3.4 on satijalab.org/ran
    desc.file <- file(description = paste0(repo, '/src/contrib/PACKAGES'))
    # Taken from remotes:::read_dcf
    fields <- colnames(x = read.dcf(file = desc.file))
    seurat.desc <- as.list(x = read.dcf(
      file = desc.file,
      keep.white = fields
    )[1, ])
    close(con = desc.file)
    # Taken from remotes:::parse_deps
    seurat.deps <- lapply(
      X = seurat.desc[c('Depends', 'Imports', 'LinkingTo')],
      FUN = function(x) {
        parts <- unlist(x = strsplit(x = x, split = '[[:space:]]*,[[:space:]]*'))
        names <- gsub(
          pattern = '^\\s+|\\s^$',
          replacement = '',
          x = gsub(
            pattern = '\\s*\\(.*?\\)',
            replacement = '',
            x = parts
          )
        )
        versions <- parts
        versions[!grepl(pattern = '\\(.*\\)', x = versions)] <- NA
        compare <- sub(
          pattern = '.*\\(\\s*(\\S+)\\s+.*\\s*\\)',
          replacement = '\\1',
          x = versions
        )
        versions <- sub(
          pattern = '.*\\(\\s*\\S+\\s+(\\S*)\\s*\\)',
          replacement = '\\1',
          versions
        )
        deps <- data.frame(
          names = names,
          compare = compare,
          version = versions,
          stringsAsFactors = FALSE
        )
        return(deps)
      }
    )
    seurat.deps <- do.call(
      what = 'rbind',
      args = c(seurat.deps, make.row.names = FALSE)
    )
    # Get R version information
    min.version <- seurat.deps[seurat.deps$names == 'R', 'version']
    r.version <- package_version(x = paste(R.Version()[c('major', 'minor')], collapse = '.'))
    # Get package type
    type <- getOption(x = 'pkgType', default = 'source')
    # Simplify package type
    if (grepl(pattern = 'mac\\.binary', x = type) && type != 'mac.binary.el-capitan') {
      type <- 'source'
    } else if (type == 'both') {
      type <- switch(
        EXPR = .Platform$OS.type,
        'windows' = 'win.binary',
        'mac.binary.el-capitan'
      )
    } else if (type == 'binary') {
      type <- if (.Platform$OS.type == 'windows') {
        'win.binary'
      } else if (Sys.info()$sysname == 'Darwin' && package_version(x = Sys.info()$release) > package_version(x = '15.0.0')) {
        'mac.binary.el-capitan'
      } else {
        'source'
      }
    }
    # Figure out the URL for binaries
    repo.platform <- switch(
      EXPR = type,
      'win.binary' = 'windows',
      'mac.binary.el-capitan' = 'macosx/el-capitan',
      NA
    )
    # Ensure that we can actually install this version of Seurat
    if (r.version < package_version(x = min.version)) {
      stop("Seurat v", remote.version, " requires R ", min.version, " or higher", call. = FALSE)
    } else if (!is.na(x = repo.platform)) {
      tryCatch(
        expr = {
          test <- file(description = paste(
            repo,
            'bin',
            repo.platform,
            'contrib',
            tools::file_path_sans_ext(x = r.version),
            'PACKAGES',
            sep = '/'
          ))
          tmp <- suppressWarnings(expr = read.dcf(file = test))
          close(con = test)
        },
        error = function(...) {
          warning(
            "Seurat v",
            remote.version,
            " binaries for ",
            switch(EXPR = type, 'win.binary' = 'Windows', 'macOS'),
            " have not been built for R ",
            r.version,
            ", installing from source instead",
            immediate. = TRUE,
            call. = FALSE
          )
          close(con = test)
          type <<- 'source'
        }
      )
    }
    # Remove base packages
    seurat.deps <- seurat.deps[seurat.deps$names != 'R', ]
    inst.pkgs <- utils::installed.packages()
    base.pkgs <- unname(obj = inst.pkgs[inst.pkgs[, 'Priority'] %in% c('base', 'recommended', 'Package'), 'Package'])
    seurat.deps <- subset(
      x = seurat.deps,
      subset = names %in% setdiff(x = seurat.deps$names, y = base.pkgs)
    )
    # Find which dependencies we need to install
    deps.to.install <- apply(
      X = seurat.deps,
      MARGIN = 1,
      FUN = function(x) {
        x <- unname(obj = x)
        dep <- x[1]
        comp <- x[2]
        req <- x[3]
        inst <- tryCatch(
          expr = as.character(x = utils::packageVersion(pkg = dep)),
          error = function(...) {
            return(NULL)
          }
        )
        if (!is.null(x = inst)) {
          dep <- if (is.na(x = comp)) {
            NA
          } else {
            ifelse(
              test = do.call(
                what = comp,
                args = list(package_version(x = inst), package_version(x = req))
              ),
              yes = NA,
              no = dep
            )
          }
        }
        return(dep)
      }
    )
    deps.to.install <- unname(obj = as.character(x = na.omit(object = deps.to.install)))
    if (length(x = deps.to.install) > 0) {
      install.packages(pkgs = deps.to.install)
    }
    install.packages('Seurat', repos = 'https://satijalab.org/ran', type = type)
  }
})

tryCatch(
  expr = attachNamespace(ns = 'Seurat'),
  error = function(...) {
    return(invisible(x = NULL))
  }
)
