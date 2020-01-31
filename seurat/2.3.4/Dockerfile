FROM r-base:3.5.3

# Install ssl, curl, ssh, and git
RUN apt-get update -qq && \
	apt-get install -t unstable -y libssl-dev libcurl4-openssl-dev libssh2-1-dev libhdf5-dev gfortran && \
	apt-get install --no-install-recommends -y \
		build-essential \
		automake \
		git \
		zlib1g-dev \
		libxml2-dev \
		cmake \
		gnupg \
		lsb-release && \
	apt-get clean

## INSTALL SEURAT 2.3.4 ##
ADD archived_seurat.R /software/archived_seurat.R
ADD install.R /software/install.R
RUN Rscript /software/install.R

## INSTALL FAST_TSNE ##
RUN wget http://www.fftw.org/fftw-3.3.8.tar.gz && \
	tar -zxvf fftw-3.3.8.tar.gz && \
	mv fftw-3.3.8/ /software/fftw-3.3.8 && \
	rm fftw-3.3.8.tar.gz && \
	cd /software/fftw-3.3.8 && \
	./configure && \
	make && \
	make install && \
	cd ../.. && \
	git clone https://github.com/KlugerLab/FIt-SNE.git /software/FIt-SNE && \
	cd /software/FIt-SNE && \
	g++ -std=c++11 -O3  src/sptree.cpp src/tsne.cpp src/nbodyfft.cpp  -o bin/fast_tsne -pthread -lfftw3 -lm && \
	cd ../..

ENV PATH=$PATH:/software/FIt-SNE/bin

## Install Python with anndata
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
	bash ./Miniconda3-latest-Linux-x86_64.sh -b

ENV PATH=/root/miniconda3/bin:$PATH
ARG old_path=$PATH

RUN conda update -n base -c defaults conda
RUN conda create -n anndata -y python=3.7

ENV PATH=/root/miniconda3/envs/anndata/bin:$old_path

RUN pip install anndata && \
    pip install loompy

ENV PATH=$old_path

WORKDIR "software"
CMD ["bash"]