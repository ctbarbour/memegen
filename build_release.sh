tarball=$(./rebar3 as prod tar | grep tarball | awk '{print $3}')
cp $tarball /data/
