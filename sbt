#!/bin/sh

java -Xmx512M -XX:PermSize=256M $SBTOPTS -jar $(dirname $0)/sbt-launch.jar "$@"
