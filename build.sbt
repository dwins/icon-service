organization := "com.example"

name := "icon-service"

version := "0.1.0-SNAPSHOT"

resolvers ++= Seq(
  "osgeo" at "http://download.osgeo.org/webdav/geotools/")

libraryDependencies ++= Seq(
   "net.databinder" %% "unfiltered-filter" % "0.6.5",
   "javax.servlet" % "servlet-api" % "2.3" % "provided",
   "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "container",
   "org.clapper" % "avsl_2.9.1" % "0.3.6",
   "org.geotools" % "gt-main" % "8.5",
   "org.geotools" % "gt-shapefile" % "8.5",
   "org.geotools" % "gt-epsg-hsql" % "8.5",
   "org.geotools" % "gt-render" % "8.5"
)

seq(webSettings :_*)

port in container.Configuration := 9090
