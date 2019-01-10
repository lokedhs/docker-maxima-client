Maxima-Client Appimage build
============================

This is a docker container that is used to build the Appimage version
of Climaxima.

Precompiled release
-------------------

Precompiled binaries can be dowloaded directly from the releases
section on github:
https://github.com/lokedhs/docker-maxima-client/releases

Building Appimage
-----------------

To build the Appimage, run the following command:

```
$ docker build -t maxima .
```

Assuming everything built correctly, run the following command to copy
the Appimage from the container:

```
$ docker run maxima cat climaxima-x86_64.AppImage > climaxima-x86_64.AppImage
```

Finally, set the executable flag so that the executable can be run:

```
$ chmod +x climaxima-x86_64.AppImage
```
