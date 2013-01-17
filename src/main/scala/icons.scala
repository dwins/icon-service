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
        sym <- rule.symbolizers.asScala
        if sym.isInstanceOf[org.opengis.style.PointSymbolizer]
        ptSym = sym.asInstanceOf[org.opengis.style.PointSymbolizer]
        graphic = ptSym.getGraphic
        if graphic != null
        if graphic.getSize != null
      } yield graphic.getSize.evaluate(feature, classOf[Int])
    graphicSizes.reduceOption(_ max _).getOrElse(16)
  }

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
}
