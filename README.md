# IconService

IconService is an experiment in automatic faithful representation of SLD
styling in KML documents.  Since SLD has a much richer set of styling tools
than KML, it is doomed to failure.  However, it's interesting to see how close
we can get :)  Eventually the ideas in IconService will be integrated into [GeoServer](http://geoserver.org/).

## Building and Running
IconService is built using [SBT](http://scala-sbt.org/).  SBT is distributed as a single executable JAR, which is actually included in this Git repository for convenience.  When run, it will download any further needed components (this might take a while the first time, but subsequent runs will be faster.)  To get a web server running IconService at http://localhost:9090/ just run:

    ./sbt '~; container:start; container:reload /'

This will run an embedded Jetty servlet container and automatically redeploy when code is changed in the src/ directory.  To stop it, press Enter or Ctrl+C at the terminal.  For other SBT commands, run `./sbt` with no arguments to get an interactive prompt.  Enter 'help' into this console for instructions and a list of common tasks.

## Usage
In order for IconService to do anything useful, you should have some map styles in SLD format in the `styles` directory and some map data in Shapefile format in the `data` directory. (Subdirectories aren't supported.)

For dynamically rendered icons, issue requests like:

    curl http://localhost:9090/st/{style}?att1=val1,att2=val2

{style} should be the name (without .sld) of a style file in the styles/ directory.  Any query parameters will be treated as attribute values (which will be used for any `<PropertyName>` elements in the SLD).

For KML, issue requests like:

    curl http://localhost:8080/kml/{style}/{data}.kml

{style} is once again the name (without .sld) of a stlye file in the styles/ directory.  {data} on the other hand is the name of a shapefile in the data/ directory (also without the file extension).  I recommend accessing this url in Google Earth instead of with curl though!  You can switch the suffix to .kmz for a KMZ file with dynamic icons embedded.

## Components

IconService is built using [Scala](http://docs.scala-lang.org), [Unfiltered](http://unfiltered.databinder.net), and [GeoTools](http://geotools.org/).
