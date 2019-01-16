FROM debian:stable

RUN apt-get update && apt-get -y install git autoconf python binutils \
    texinfo gcc cmake libtool vim desktop-file-utils pkgconf libcairo2-dev \
    libssl-dev libfuse-dev zsync wget fuse bzip2 gawk g++ gperf ghostscript mupdf

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

RUN wget 'http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.15-x86-64-linux-binary.tar.bz2' -O /tmp/sbcl.tar.bz2 && \
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
    git checkout bdf73e5f41b4a493957ebb118649d954a2d71ec8

RUN cd maxima-code && \
    mkdir dist && \
    ./bootstrap && \
    ./configure --enable-sbcl --prefix=`pwd`/dist && \
    make && \
    make install

RUN git clone https://github.com/lokedhs/maxima-client.git && \
    cd maxima-client && \
    git checkout 564ce254916ad4a426770c867e858b351bae4076

RUN git clone https://github.com/McCLIM/McCLIM.git && \
    cd McCLIM && \
    git checkout 7df0a73a280428a127de854080d365289016d69b
RUN sed -i 's/"libfontconfig\.so"/(:or "libfontconfig\.so\.1" "libfontconfig\.so")/' McCLIM/Extensions/fontconfig/src/functions.lisp
RUN sed -i 's/"libharfbuzz\.so"/(:or "libharfbuzz\.so\.0" "libharfbuzz\.so")/' McCLIM/Extensions/harfbuzz/src/functions.lisp

RUN git clone https://github.com/lokedhs/cl-freetype2 && \
    cd cl-freetype2 && \
    git checkout fe9d8108154cb4f4e46622bcaea4ba6155a3b5af

RUN ln -s /maxima-code /root/quicklisp/local-projects/maxima-code && \
    ln -s /maxima-client /root/quicklisp/local-projects/maxima-client && \
    ln -s /McCLIM /root/quicklisp/local-projects/McCLIM && \
    ln -s /cl-freetype2 /root/quicklisp/local-projects && \
    echo '(pushnew :mcclim-ffi-freetype *features*)' >> /root/.sbclrc

COPY startup.lisp /
RUN sbcl --load startup.lisp

RUN cd maxima-client/infoparser && \
    ./build-binary.sh

COPY mkdoc.lisp /
RUN sbcl --load mkdoc.lisp

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
    cp -r /maxima-client/fonts /maxima-client/images maxima-client && \
    mkdir maxima-client/infoparser

RUN cp -r /maxima-client/infoparser/docs /maxima-client/infoparser/figures maxima-client/infoparser

COPY fonts/* maxima-client/fonts/tex/

RUN mkdir -p usr/share/metainfo
COPY climaxima.appdata.xml usr/share/metainfo/

COPY AppRun .
RUN chmod +x AppRun
COPY climaxima.desktop .
COPY maxima.png .

WORKDIR /
RUN ARCH=x86_64 appimagetool maxima-squashfs
