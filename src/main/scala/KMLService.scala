package dwins.icons

import scala.collection.JavaConverters._

import unfiltered.request._
import unfiltered.response._

import org.clapper.avsl.Logger

class KMLService extends unfiltered.filter.Plan {
  object KML {
    def unapply(s: String): Option[String] =
      if (s endsWith ".kml")
        Some(s dropRight 4)
      else
        None
  }
  object KMZ {
    def unapply(s: String): Option[String] =
      if (s endsWith ".kmz")
        Some(s dropRight 4)
      else
        None
  }

  def intent = {
    case GET(Path(Seg("kml" :: styleName :: KMZ(dataName) :: Nil))) & HostPort(host, port) =>
      val prefix = "http://" + host + ":" + port + "/st/" + styleName
      def constructKMZ(style: Style) = 
        withFeatureSource(dataName) { source =>
          val temp = java.io.File.createTempFile("dwins.icons", "kmz")
          withOutStream(temp) { out =>
            withZipOutStream(out)(writeKMZ(prefix, style, source, _))
          }
          temp
        }
      val expected =
        for {
          sty <- readStyle(styleName).right
          kmzFile <- constructKMZ(sty).right
        } yield kmzFile
      expected match {
        case Left(err) =>
          InternalServerError ~> PlainTextContent ~> ResponseString(err)
        case Right(kmzFile) =>
          Ok ~> ContentType("application/vnd.google-earth.kmz") ~> file(kmzFile)
      }
    case GET(Path(Seg("kml" :: styleName :: KML(dataName) :: Nil))) & HostPort(host, port) =>
      val prefix = "http://" + host + ":" + port + "/st/" + styleName
      def constructKML(style: Style) = 
        withFeatureSource(dataName) { source =>
          val temp = java.io.File.createTempFile("dwins.icons", "kml")
          withWriter(temp)(writeKML(prefix, style, source, _))
          temp
        }
      val expected =
        for {
          sty <- readStyle(styleName).right
          kmlFile <- constructKML(sty).right
        } yield kmlFile
      expected match {
        case Left(err) =>
          InternalServerError ~> PlainTextContent ~> ResponseString(err)
        case Right(kmlFile) =>
          Ok ~> ContentType("application/vnd.google-earth.kml+xml") ~> file(kmlFile)
      }
  }

  val inLatLon = {
    val q = new org.geotools.data.Query
    q.setCoordinateSystemReproject(
      org.geotools.referencing.CRS.decode("EPSG:4326"))
    q
  }

  def writeKMZ(iconPrefix: String, style: Style, features: Iterator[Feature], zipOut: java.util.zip.ZipOutputStream): Unit = {
    val entry = new java.util.zip.ZipEntry(_: String)
    zipOut.putNextEntry(entry("doc.kml"))
    withStreamWriter(zipOut)(writeKML(iconPrefix, style, features, _))
  }

  def writeKML(iconPrefix: String, style: Style, features: Iterator[Feature], writer: java.io.Writer): Unit = {
    val kml =
      <kml xmlns="http://www.opengis.net/kml/2.2"
           xmlns:gs="http://www.google.com/kml/ext/2.2">
        <Document>
          { placemarks(iconPrefix, style, features) }
        </Document>
      </kml>
    scala.xml.XML.write(writer, kml, enc="UTF-8", xmlDecl=true, doctype=null)
  }

  def placemarks(iconPrefix: String, style: Style, features: Iterator[Feature]): scala.xml.NodeSeq = {
    import scala.xml._

    NodeSeq.Empty ++
    features.map { f =>
      val centroid = f.getDefaultGeometry.asInstanceOf[com.vividsolutions.jts.geom.Geometry].getCentroid
      <Placemark>
        <name>{f.getID}</name>
        <Style>
          { iconStyles(iconPrefix, style, f) }
        </Style>
        <Point>
          <coordinates>{"%s,%s".format(centroid.getY, centroid.getX)}</coordinates>
        </Point>
      </Placemark>
    }
  }

  def embeds(iconPrefix: String, style: Style, features: Iterator[Feature]): Map[String, Map[String, String]] = {
    def hash(m: Map[String, String]): String = {
      val digester = java.security.MessageDigest.getInstance("md5")
      for ((k, v) <- m.toSeq.sorted) {
        digester.digest(k.getBytes("UTF-8"))
        digester.digest(v.getBytes("UTF-8"))
      }
      digester.digest()
        .view
        .map("%x".format(_))
        .mkString
    }

    val infos: Iterator[Map[String, String]] =
      for {
        feature <- features
        if iconInfo(style, feature).isRight
      } yield asMap(feature)
    infos.map(m => (hash(m), m)).toMap
  }

  case class ExternalRef(url: String, heading: Double, opacity: Double)
  case class DynamicIcon(feature: Feature)

  def iconInfo(style: Style, feature: Feature): Either[ExternalRef, DynamicIcon] = {
    val staticHeadingOpacityAndPublicUrl = 
      for {
        (h, o) <- staticHeading(style, feature)
        u <- publicUrl(style, feature)
      } yield ExternalRef(url = u, heading = h, opacity = o)

    staticHeadingOpacityAndPublicUrl.toLeft(DynamicIcon(feature))
  }

  def iconStyles(iconPrefix: String, style: Style, feature: Feature): scala.xml.NodeSeq = {
    def mkStyleElement(styleInfo: Either[ExternalRef, DynamicIcon]): scala.xml.Elem =
      styleInfo match {
        case Left(ExternalRef(href, heading, opacity)) =>
          val colorMask = "#02xffffff" format math.round(opacity * 255)
          <IconStyle>
            <color>{ colorMask }</color>
            <heading>{ heading }</heading>
            <Icon>
              <href>{ href }</href>
            </Icon>
          </IconStyle>
        case Right(DynamicIcon(feature)) =>
          val href = styleHref(iconPrefix, style, feature)
          <IconStyle>
            <Icon>
              <href>{ href }</href>
            </Icon>
          </IconStyle>
      }
    mkStyleElement(iconInfo(style, feature))
  }

  def scaleValue(style: Style, feature: Feature): Double =
    iconSize(style, feature) / 16d

  def asMap(f: Feature): Map[String, String] =
    f.getProperties.asScala
     .map { p => (p.getName.getLocalPart, String.valueOf(p.getValue)) }
     .toMap 

  def styleHref(iconPrefix: String, style: Style, feature: Feature): String = {
    import java.net.URLEncoder.encode
    def query(m: Map[String, String]): String =
      m.filterKeys(_ != feature.getFeatureType.getGeometryDescriptor.getLocalName)
       .map { case (k, v) => "%s=%s".format(k, encode(v, "UTF-8")) }
       .mkString("&")
    val q = query(asMap(feature))
    if (q.isEmpty)
      iconPrefix
    else
      iconPrefix + "?" + q
  }

  def staticHeading(style: Style, feature: Feature): Option[(Double, Double)] = {
    val graphics: Seq[org.geotools.styling.Graphic] = 
      for {
        ft <- style.featureTypeStyles.asScala
        r <- ft.rules.asScala
        s <- r.symbolizers.asScala
        if s.isInstanceOf[org.geotools.styling.PointSymbolizer]
      } yield s.asInstanceOf[org.geotools.styling.PointSymbolizer].getGraphic

    Some(graphics)
      .collect { case Seq(g) => 
        val rotationExp = g.getRotation
        val opacityExp = g.getOpacity
        val rawRotation = rotationExp.evaluate(feature, classOf[Double])
        val clampedRotation = (rawRotation % 360d)
        val rotation = 
          if (clampedRotation <= 0)
            clampedRotation + 360d
          else
            clampedRotation
        val opacity = opacityExp.evaluate(feature, classOf[Double]) 
        (rotation, opacity)
      }
  }

  def publicUrl(style: Style, feature: Feature): Option[String] = {
    val graphics: Seq[org.opengis.style.GraphicalSymbol] = 
      for {
        ft <- style.featureTypeStyles.asScala
        r <- ft.rules.asScala
        s <- r.symbolizers.asScala
        if s.isInstanceOf[org.geotools.styling.PointSymbolizer]
        ps = s.asInstanceOf[org.geotools.styling.PointSymbolizer]
        g = ps.getGraphic
        symbol <- g.graphicalSymbols.asScala
      } yield symbol
    Some(graphics)
      .collect { case Seq(exGraphic: org.geotools.styling.ExternalGraphic) =>
        val url = exGraphic.getLocation().toExternalForm()
        val expression = org.geotools.renderer.style.ExpressionExtractor.extractCqlExpressions(url)
        val effectiveUrl = expression.evaluate(feature, classOf[String])
        effectiveUrl
      }.filter(imageBlackList)
  }

  def withWriter[T]
    (file: java.io.File)
    (op: java.io.Writer => T): T =
  {
    val writer = new java.io.FileWriter(file)
    try
      op(writer)
    finally
      writer.close()
  }

  def withStreamWriter[T]
    (out: java.io.OutputStream)
    (op: java.io.Writer => T): T = 
  {
    val writer = new java.io.OutputStreamWriter(out)
    try
      op(writer)
    finally
      writer.close()
  }

  def withOutStream[T]
    (file: java.io.File)
    (op: java.io.OutputStream => T): T =
  {
    val writer = new java.io.FileOutputStream(file)
    try
      op(writer)
    finally
      writer.close()
  }

  def withZipOutStream[T]
    (outStream: java.io.OutputStream)
    (op: java.util.zip.ZipOutputStream => T): T =
  {
    val zipStream = new java.util.zip.ZipOutputStream(outStream)
    try
      op(zipStream)
    finally
      zipStream.close()
  }

  def file(path: java.io.File): ResponseStreamer =
    new ResponseStreamer {
      def stream(writer: java.io.OutputStream) {
        val reader = new java.io.FileInputStream(path)
        val buff = Array.ofDim[Byte](8192)
        var len = 0
        try
          while ({len = reader.read(buff); len >= 0})
            writer.write(buff, 0, len)
        finally
          reader.close()
      }
    }

  def withFeatureSource[T]
    (name: String)
    (op: Iterator[Feature] => T): Either[String, T] =
  {
    val dataDir = new java.io.File("data").toURI.toURL
    val params: java.util.Map[_,_] = Map("url" -> dataDir).asJava

    val fac = new org.geotools.data.shapefile.ShapefileDataStoreFactory
    val store = fac.createDataStore(params)

    try {
      val source = store.getFeatureSource(name)
      val features = source.getFeatures(inLatLon).features
      val iter = Iterator.continually(features.next).takeWhile(_ => features.hasNext)
      try
        Right(op(iter))
      catch {
        case (e: java.io.IOException) => Left(e.getMessage)
      }
      finally 
        features.close()
    } finally
      store.dispose()
  }
}
