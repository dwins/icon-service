package dwins

import scala.collection.JavaConverters._

package object icons {
  val styleFactory = org.geotools.factory.CommonFactoryFinder.getStyleFactory

  type Style = org.geotools.styling.Style
  type Schema = org.opengis.feature.simple.SimpleFeatureType
  type Feature = org.opengis.feature.simple.SimpleFeature
  type FeatureSource = org.geotools.data.FeatureSource[Schema, Feature]

  def readStyle(name: String): Either[String, Style] = {
    val fullPath = name + ".sld"
    val here = new java.io.File("styles")
    val file = new java.io.File(here, fullPath)
    if (file.getCanonicalFile.getParentFile != here.getCanonicalFile)
      Left("Disallowed file path " + fullPath)
    else {
      try {
        val parser = new org.geotools.styling.SLDParser(styleFactory, file)
        val styles = parser.readXML()
        if (styles.isEmpty)
          Left("No styles in that file")
        else
          Right(styles.head)
      } catch {
        case (e: java.io.IOException) => Left(e.getMessage)
      }
    }
  }

  def iconSize(style: Style, feature: Feature): Int = {
    val graphicSizes = 
      for {
        ftStyle <- style.featureTypeStyles.asScala
        rule <- ftStyle.rules.asScala
        if applicable(feature)(rule)
        sym <- rule.symbolizers.asScala
        if sym.isInstanceOf[org.opengis.style.PointSymbolizer]
        ptSym = sym.asInstanceOf[org.opengis.style.PointSymbolizer]
        graphic = ptSym.getGraphic
        if graphic != null
        if graphic.getSize != null
      } yield graphic.getSize.evaluate(feature, classOf[Int])
    println(graphicSizes)
    graphicSizes.reduceOption(_ max _).getOrElse(16)
  }

  def applicable(f: Feature): org.geotools.styling.Rule => Boolean =
    r => Option(r.getFilter).forall(_ evaluate f)

  /**
   * True iff the given URL should be included verbatim in KML output.
   */
  def imageBlackList(urlString: String): Boolean = {
    import scala.util.control.Exception.catching, java.net.MalformedURLException
    val goodSchemes = Set("http", "https")
    val badHosts = Set("localhost", "geoserver", "chart")
    catching(classOf[MalformedURLException])
      .opt { new java.net.URL(urlString) }
      .forall { url =>
        goodSchemes.contains(url.getProtocol) && !badHosts.contains(url.getHost)
      }
  }

  def draw(style: Style, params: Map[String, String]) = {
    val (col, feature) = sample(params)
    val graphics = 
      for {
        ftStyle <- style.featureTypeStyles.asScala
        rule <- ftStyle.rules.asScala
        sym <- rule.symbolizers.asScala
        if sym.isInstanceOf[org.opengis.style.PointSymbolizer]
      } yield sym.asInstanceOf[org.opengis.style.PointSymbolizer].getGraphic
    def gsize(g: org.opengis.style.Graphic): Int =
      (for {
        graphic <- Option(g)
        sizeExp <- Option(graphic.getSize)
      } yield sizeExp.evaluate(feature, classOf[Int])).getOrElse(16)
    val size = graphics.map(gsize).max
    render(size + 2, style, col)
  }

  def sample(params: Map[String, String]): 
    Pair[org.geotools.data.simple.SimpleFeatureCollection,
         org.opengis.feature.simple.SimpleFeature] = 
  {
    val schemaBuilder = new org.geotools.feature.simple.SimpleFeatureTypeBuilder
    schemaBuilder.setName("example")
    schemaBuilder.setNamespaceURI("http://example.com/")
    schemaBuilder.setSRS("EPSG:4326")
    params.keySet.foreach { k => schemaBuilder.add(k, classOf[String]) }
    schemaBuilder.add("the_geom", classOf[com.vividsolutions.jts.geom.Point])
    val schema = schemaBuilder.buildFeatureType()
    val featureBuilder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
    params.foreach {
      case (k, v) => featureBuilder.set(k, v)
    }
    featureBuilder.set("the_geom", point(0,0))
    val feature = featureBuilder.buildFeature(null)
    val collection = new org.geotools.data.memory.MemoryFeatureCollection(schema)
    collection.add(feature)
    (collection, feature)
  }

  def render(size: Int, style: Style, features: org.geotools.data.simple.SimpleFeatureCollection) = {
    val raster = mkImage(size)
    val screenBounds = new java.awt.Rectangle(0, 0, size, size)
    val latLon = org.geotools.referencing.CRS.decode("EPSG:4326")
    val worldBounds = new org.geotools.geometry.jts.ReferencedEnvelope(-1, 1, -1, 1, latLon)
    val renderer = new org.geotools.renderer.lite.StreamingRenderer
    val mapContent = new org.geotools.map.MapContent
    mapContent.layers.add(
      new org.geotools.map.FeatureLayer(features, style))
    renderer.setMapContent(mapContent)
    val graphics = raster.createGraphics()
    graphics.setRenderingHint(
      java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    renderer.paint(graphics, screenBounds, worldBounds)
    graphics.dispose()
    mapContent.dispose()
    raster
  }

  def mkImage(size: Int): java.awt.image.BufferedImage = {
    import java.awt.image.BufferedImage
    new BufferedImage(size, size, BufferedImage.TYPE_INT_ARGB)
  }

  def point(x: Double, y: Double): com.vividsolutions.jts.geom.Point = {
    val geometryFactory = new com.vividsolutions.jts.geom.GeometryFactory
    val coord = new com.vividsolutions.jts.geom.Coordinate
    geometryFactory.createPoint(coord)
  }

}
