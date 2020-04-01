#!/bin/bash

# do not run

basedir=`dirname $0`
basedir=`readlink -f $basedir/../`
cd $basedir || exit 1

version=`head -n1 VERSION`
name=ecaml
tar=$name-$version.tar.gz
tar_dst=$HOME/htdocs/src/$name
chroot_dir=/home/komar/chroot/squeeze-x86
chroot_dist_dir=/home/komar/$name
user=komar
ecaml_byte=$name-bytecode-$version
ecaml_opt=$name-bin-x86-$version

function release_sources {
  darcs dist -d $name-$version || exit 1
  cp $tar $tar_dst || exit 1
  make -s clean || exit 1
}

function release_ecaml {
  cd $chroot_dist_dir || exit 1
  make -s clean || exit 1

  make -s -j4 ${name} || exit 1
  mkdir -p $ecaml_byte
  mv ${name} $ecaml_byte || exit 1
  tar -cf $ecaml_byte.tar.gz $ecaml_byte || exit 1
  rm -r $ecaml_byte

  make -s -j4 ${name}.opt || exit 1
  mkdir -p $ecaml_opt
  strip --strip-unneeded ${name}.opt || exit 1
  mv ${name}.opt $ecaml_opt || exit 1
  tar -cf $ecaml_opt.tar.gz $ecaml_opt || exit 1
  make -s clean || exit 1
  rm -r $ecaml_opt

  make -s clean || exit 1

  exit
}

case "$1" in
  sources) release_sources;;
  ecaml-root) su -c "$0 ecaml" $user;;
  ecaml) release_ecaml;;
  *) $0 sources &&
    rm -rf $chroot_dir/$chroot_dist_dir &&
    darcs put --no-set-default $chroot_dir/$chroot_dist_dir &&
    chmod +x $chroot_dir/$chroot_dist_dir/devel/release.bash &&
    sudo chroot $chroot_dir $chroot_dist_dir/devel/release.bash ecaml-root;
    mkdir -p $tar_dst/builded/;
    cp $chroot_dir/$chroot_dist_dir/$ecaml_byte.tar.gz $tar_dst/builded/;
    cp $chroot_dir/$chroot_dist_dir/$ecaml_opt.tar.gz $tar_dst/builded/;;
esac

