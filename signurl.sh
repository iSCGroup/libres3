#!/bin/bash
OBJECT_PATH="$1"
EXPIRES=1452191468
#$(date +%s --date "now 100000000 seconds")
AWS_ACCESS_KEY_ID=LIBRES3
STRINGTOSIGN=$(echo -e "GET\n\n\n$EXPIRES\n/$OBJECT_PATH")
SIGNATURE=$(s3cmd -c /opt/lsb-skylable.com-libres3/share/doc/libres3/libres3.sample.s3cfg sign "$STRINGTOSIGN")
SIGNATURE=${SIGNATURE#Signature: }
SIGNATURE="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$SIGNATURE")"
echo $STRINGTOSIGN
echo "curl -O 'http://libres3.skylable.com:8008/$OBJECT_PATH?AWSAccessKeyId=$AWS_ACCESS_KEY_ID&Expires=$EXPIRES&Signature=$SIGNATURE'"
STRINGTOSIGN=$(echo "PUT\n\n\n$EXPIRES\n/$OBJECT_PATH")
SIGNATURE=$(s3cmd -c /opt/lsb-skylable.com-libres3/share/doc/libres3/libres3.sample.s3cfg sign "$STRINGTOSIGN")
SIGNATURE=${SIGNATURE#Signature: }
SIGNATURE="$(perl -MURI::Escape -e 'print uri_escape($ARGV[0]);' "$SIGNATURE")"
echo "curl -T /path/to/source -O 'http://libres3.skylable.com:8008/$OBJECT_PATH?AWSAccessKeyId=$AWS_ACCESS_KEY_ID&Expires=$EXPIRES&Signature=$SIGNATURE'"
