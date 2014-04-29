#!/bin/sh
set -e
SXDIR=../sx
. libres3/setup.data
PIDFILE=$localstatedir/run/libres3/libres3.pid
cleanup() {
    echo "Killing libres3_ocsigen"
    $bindir/libres3_ocsigen --stop
}
trap cleanup INT TERM EXIT
rm -f $PIDFILE
echo "Starting SX"
#(cd $SXDIR/server && sudo test/start-nginx.sh)
echo
echo "Installing ocsigen"
make reinstall
$bindir/libres3_ocsigen --version
echo "Configuring ocsigen"
$bindir/libres3_setup
echo "Starting ocsigen"
$bindir/libres3_ocsigen
echo "Running tests"
libres3/netTest.native --s3cfg $docdir/libres3.sample.s3cfg --backtrace 2>&1 | tee sx.log
echo "OK"
