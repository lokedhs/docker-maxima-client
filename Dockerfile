FROM debian:stable

RUN apt-get update && apt-get -y install git autoconf python binutils texinfo gcc

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
RUN apt-get -y install gawk
RUN apt-get -y install g++
RUN apt-get -y install gperf

RUN wget 'https://mirrors.edge.kernel.org/pub/linux/utils/util-linux/v2.33/util-linux-2.33.tar.gz'
RUN zcat util-linux-2.33.tar.gz | tar xvf -
RUN cd util-linux-2.33 && \
    ./configure --disable-all-programs --enable-libuuid && \
    make && \
    make install && \
    ldconfig

RUN wget 'https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-2.1.3.tar.bz2'
RUN bzcat harfbuzz-2.1.3.tar.bz2 | tar xf -
RUN cd  harfbuzz-2.1.3 && \
    ./configure && \
    make && \
    make install && \
    ldconfig

RUN wget 'https://download.savannah.gnu.org/releases/freetype/freetype-2.9.tar.bz2'
RUN bzcat freetype-2.9.tar.bz2 | tar xf -
RUN cd freetype-2.9 && \
    ./configure && \
    make && \
    make install && \
    ldconfig

RUN wget 'https://www.freedesktop.org/software/fontconfig/release/fontconfig-2.13.1.tar.bz2'
RUN bzcat fontconfig-2.13.1.tar.bz2 | tar xf -
RUN cd fontconfig-2.13.1 && \
    ./configure && \
    make && \
    make install && \
    ldconfig

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

RUN ln -s /cl-freetype2 /root/quicklisp/local-projects

RUN git clone https://git.code.sf.net/p/maxima/code maxima-code && \
    cd maxima-code && \
    git checkout 4e6a6c6ef36c31b979ffb22b0ee089e966ff5452

RUN cd maxima-code && \
    mkdir dist && \
    ./bootstrap && \
    ./configure --enable-sbcl --prefix=`pwd`/dist && \
    make && \
    make install

RUN git clone https://github.com/lokedhs/maxima-client.git && \
    cd maxima-client && \
    git checkout e9d18ba6308d19cb89ad0e441f5b039ce9660cfa

RUN git clone https://github.com/McCLIM/McCLIM.git && \
    cd McCLIM && \
    git checkout dd09173cdf744982043cc9b3627384872e6535e9
RUN sed -i 's/"libfontconfig\.so"/(:or "libfontconfig\.so\.1" "libfontconfig\.so")/' McCLIM/Extensions/fontconfig/src/functions.lisp
RUN sed -i 's/"libharfbuzz\.so"/(:or "libharfbuzz\.so\.0" "libharfbuzz\.so")/' McCLIM/Extensions/harfbuzz/src/functions.lisp

RUN git clone https://github.com/lokedhs/cl-freetype2 && \
    cd cl-freetype2 && \
    git checkout fe9d8108154cb4f4e46622bcaea4ba6155a3b5af

RUN ln -s /maxima-code /root/quicklisp/local-projects/maxima-code && \
    ln -s /maxima-client /root/quicklisp/local-projects/maxima-client && \
    ln -s /McCLIM /root/quicklisp/local-projects/McCLIM && \
    echo '(pushnew :mcclim-ffi-freetype *features*)' >> /root/.sbclrc

COPY startup.lisp /
RUN sbcl --load startup.lisp

COPY appimagetool-x86_64.AppImage /
RUN chmod +x appimagetool-x86_64.AppImage
RUN ./appimagetool-x86_64.AppImage --appimage-extract && \
    cp -R squashfs-root/* .

RUN mkdir maxima-squashfs
WORKDIR maxima-squashfs

RUN mkdir lib && \
    cp /usr/local/lib/libharfbuzz.so lib && \
    ln -s libharfbuzz.so lib/libharfbuzz.so.0 && \
    cp /usr/local/lib/libfreetype.so lib && \
    ln -s libfreetype.so lib/libfreetype.so.6 && \
    cp /usr/local/lib/libfontconfig.so lib && \
    ln -s libfontconfig.so lib/libfontconfig.so.1
RUN cp /usr/lib/x86_64-linux-gnu/libpng16.so lib && \
    ln -s libpng16.so lib/libpng16.so.16

RUN mkdir maxima-inst && \
    (cd ../maxima-code/dist && tar cf - *) | (cd maxima-inst && tar xf -)
RUN ln -s share/info maxima-inst/info

RUN cp /clim-maxima .
RUN mkdir maxima-client && \
    cp -r /maxima-client/fonts /maxima-client/images maxima-client
COPY fonts/* maxima-client/fonts/tex/

COPY AppRun .
RUN chmod +x AppRun
COPY climaxima.desktop .
COPY maxima.png .

WORKDIR /
RUN ARCH=x86_64 appimagetool maxima-squashfs
