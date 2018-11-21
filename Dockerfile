FROM debian:stable

RUN apt-get update && apt-get -y install git autoconf python binutils texinfo gcc libharfbuzz-dev libfontconfig1-dev libfreetype6-dev fontconfig

RUN apt-get -y install cmake
RUN apt-get -y install libtool
RUN apt-get -y install vim
RUN apt-get -y install desktop-file-utils
RUN apt-get -y install pkgconf
RUN apt-get -y install libcairo2-dev
RUN apt-get -y install libssl-dev
RUN apt-get -y install libfuse-dev
RUN apt-get -y install zsync
RUN apt-get -y install wget
RUN apt-get -y install fuse
RUN apt-get -y install bzip2

RUN wget 'http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.13-x86-64-linux-binary.tar.bz2' -O /tmp/sbcl.tar.bz2 && \
    mkdir /sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /sbcl && \
    cd /sbcl && \
    sh install.sh && \
    rm -f /tmp/sbcl.tar.bz2

RUN cd /tmp && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --load quicklisp.lisp --quit --eval '(quicklisp-quickstart:install)'
COPY sbclrc /root/.sbclrc

RUN git clone https://git.code.sf.net/p/maxima/code maxima-code && \
    cd maxima-code && \
    git checkout 380cbe206af08d17147500101bb62c899c54c851

RUN cd maxima-code && \
    mkdir dist && \
    ./bootstrap && \
    ./configure --enable-sbcl --prefix=`pwd`/dist && \
    make && \
    make install

RUN git clone https://github.com/lokedhs/maxima-client.git && \
    cd maxima-client && \
    git checkout 08a681199c5017e21af293a70127efbf9cdbf757

RUN ln -s /maxima-code /root/quicklisp/local-projects/maxima-code && \
    ln -s /maxima-client /root/quicklisp/local-projects/maxima-client && \
    ln -s /McCLIM /root/quicklisp/local-projects/McCLIM && \
    echo '(pushnew :mcclim-ffi-freetype *features*)' >> /root/.sbclrc

RUN git clone https://github.com/McCLIM/McCLIM.git && \
    cd McCLIM && \
    git checkout 29cb8b96a1e8335f99c09879f9c205db46da619a

COPY startup.lisp /
RUN sbcl --load startup.lisp

RUN wget 'https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage' && \
	chmod +x appimagetool-x86_64.AppImage
RUN ./appimagetool-x86_64.AppImage --appimage-extract && \
	cp -R squashfs-root/* .

RUN mkdir maxima-squashfs
WORKDIR maxima-squashfs

RUN mkdir maxima-inst && \
	(cd ../maxima-code/dist && tar cf - *) | (cd maxima-inst && tar xf -)
RUN ln -s share/info maxima-inst/info

RUN cp /clim-maxima .
RUN mkdir maxima-client && \
	cp -r /maxima-client/fonts /maxima-client/images maxima-client

COPY AppRun .
RUN chmod +x AppRun
COPY climaxima.desktop .
COPY maxima.png .

WORKDIR /
RUN ARCH=x86_64 appimagetool maxima-squashfs
