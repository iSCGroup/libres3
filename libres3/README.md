% LibreS3 Documentation

LibreS3 - Amazon S3 compatible server
=====================================

LibreS3 is a robust Open Source replacement for the Amazon S3 service,
implementing (a subset of) the S3 REST API.

Standard S3 client libraries and tools (for example s3cmd, python-boto,
ocaml-aws, etc.) can be used to access it. It uses Skylable SX as the
storage backend, which automatically provides data deduplication and
replication.

To get started please see the
[quickstart documentation](doc/quickstart/quickstart.pdf).

For more information visit the [home page](http://www.skylable.com/products/libres3).

If you find any bugs, please report them to our
[bugzilla](https://bugzilla.skylable.com/)

For help and support, please subscribe to our
[mailing list](http://lists.skylable.com/listinfo/sx-users)

Copyright and license
---------------------

LibreS3 is distributed under the terms of the GNU General Public License
version 2.0 with OpenSSL exception.

See [COPYING](COPYING) for more information.

LibreS3 server configuration example
------------------------------------

See the file [INSTALL.txt](INSTALL.txt) for detailed building and installation
instructions.

This example assumes that you install LibreS3 to `/opt/libres3`,
and that you have SX installed in `/opt/sx`:

~~~~
    $ ./configure --prefix=/opt/libres3 && make
    # make install
    # /opt/libres3/sbin/libres3_setup --sxsetup-conf /opt/sx/etc/sxserver/sxsetup.conf
    # /opt/libres3/sbin/libres3 start
    # /opt/libres3/sbin/libres3 status
~~~~~

Now you can access the LibreS3 server with your favourite S3 client.
For example, you can install 's3cmd' (http://s3tools.org) and use
it with the example configuration template:

~~~~
    # cp /opt/libres3/etc/libres3/libres3.sample.s3cfg ~/.s3cfg
    # s3cmd ls
~~~~

For additional examples on how to access LibreS3 with other 3rd-party
S3 clients refer to our wiki: https://wiki.skylable.com/wiki/LibreS3_Clients

You can edit `/opt/libres3/etc/libres3/libres3.conf` to change LibreS3's
configuration, and inspect the log files at `/opt/libres3/var/log/libres3/`.

Known limitations
-----------------

This is the first beta of LibreS3, and comes with some limitations
but it has been reliably running on our internal production cluster
for the past 3 months without any major issues:

  * changing bucket ACLs is not supported
  * only 90% of the S3 REST API is supported ;-)
