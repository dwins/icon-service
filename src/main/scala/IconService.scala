package dwins.icons

import scala.collection.JavaConverters._

import unfiltered.request._
import unfiltered.response._

import org.clapper.avsl.Logger

/** unfiltered plan */
class IconService extends unfiltered.filter.Plan {
  import QParams._
  val logger = Logger(classOf[IconService])

  def intent = {
    case GET(Path("/")) =>
      Ok ~> PlainTextContent ~> ResponseString("hi")
    case GET(Path(Seg("st" :: styleName :: Nil))) & Params(params) =>
    //  val expected = 
    //    for (style <- readStyle(styleName).right) yield style
    //  expected match {
    //    case Left(err) => 
    //      BadRequest ~> PlainTextContent ~> ResponseString(err)
    //    case Right(style) =>
    //      Ok ~> PlainTextContent ~> ResponseString(summarize(style))
    //  }
    //case GET(Path(Seg("st" :: styleName :: ftStyleIndex :: ruleIndex :: Nil))) & Params(params) =>
      def asInt(s:String) = 
        try
          Some(s.toInt)
        catch {
          case (e: NumberFormatException) => None
        }
      val expected =
        for {
          style <- readStyle(styleName).right
        } yield pointStyle(style)
      expected match {
        case Left(err) =>
          BadRequest ~> PlainTextContent ~> ResponseString(err)
        case Right(rule) =>
          Ok ~> ContentType("image/png") ~> ResponseBytes(png(draw(rule, params)))
      }
  }

  def pointStyle(style: Style): Style = {
    def copy
      (rule: org.geotools.styling.Rule)
      (symbolizers: Seq[org.geotools.styling.Symbolizer]): org.geotools.styling.Rule =
    {
      val newRule = styleFactory.createRule()
      newRule.setElseFilter(rule.isElseFilter)
      newRule.setFilter(rule.getFilter)
      newRule.setMaxScaleDenominator(rule.getMaxScaleDenominator)
      newRule.setMinScaleDenominator(rule.getMinScaleDenominator)
      newRule.symbolizers.addAll(symbolizers.asJava)
      newRule
    }

    def pointRule(rule: org.geotools.styling.Rule): org.geotools.styling.Rule = 
      copy(rule)(rule.symbolizers.asScala.filter { _.isInstanceOf[org.geotools.styling.PointSymbolizer] })

    val rules = 
      for {
        ft <- style.featureTypeStyles.asScala
        rule <- ft.rules.asScala
      } yield rule

    mkStyleFromRules(rules map pointRule)
  }

  def mkStyleFromRules(rules: Seq[org.geotools.styling.Rule]): Style = {
    val style = styleFactory.createStyle
    val ftStyle = styleFactory.createFeatureTypeStyle
    ftStyle.rules.addAll(rules.asJava)
    style.featureTypeStyles.add(ftStyle)
    style
  }

  def mkStyle(syms: Seq[org.geotools.styling.Symbolizer]): Style = {
    val style = styleFactory.createStyle
    val ftStyle = styleFactory.createFeatureTypeStyle
    ftStyle.rules.add(mkRule(syms))
    style.featureTypeStyles.add(ftStyle)
    style
  }

  def mkRule(syms: Seq[org.geotools.styling.Symbolizer]): org.geotools.styling.Rule = {
    val rule = styleFactory.createRule()
    syms.foreach(rule.symbolizers.add(_))
    rule
  }

  def summarize(s: Style): String = {
    val ruleNames =
      for {
        (ftStyle, ftIndex) <- s.featureTypeStyles.asScala.zipWithIndex
        (rule, ruleIndex) <- ftStyle.rules.asScala.zipWithIndex
        symbolizerText = 
          if (rule.symbolizers.asScala.exists { _.isInstanceOf[org.opengis.style.PointSymbolizer] })
            "Pointy"
          else
            "Not pointy"
      } yield ("%d.%d %s: %s" format(ftIndex, ruleIndex, rule.getName, symbolizerText))
    ruleNames.mkString("\n")
  }

  def draw(style: Style, params: Map[String, Seq[String]])/* : java.awt.image.BufferedImage */= {
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

  def sample(params: Map[String, Seq[String]]): 
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
    params.filter(_._2.nonEmpty).foreach {
      case (k, v) => featureBuilder.set(k, v.head)
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

  def png(image: java.awt.image.BufferedImage): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream
    javax.imageio.ImageIO.write(image, "PNG", out)
    out.toByteArray
  }

  def point(x: Double, y: Double): com.vividsolutions.jts.geom.Point = {
    val geometryFactory = new com.vividsolutions.jts.geom.GeometryFactory
    val coord = new com.vividsolutions.jts.geom.Coordinate
    geometryFactory.createPoint(coord)
  }

  def graphics(s: org.geotools.styling.Symbolizer): Seq[org.opengis.style.GraphicalSymbol] = {
    def graphicSymbols(g: org.opengis.style.Graphic) = 
      g match {
        case null => Nil
        case g => g.graphicalSymbols.asScala
      }

    def strokeSymbols(s: org.geotools.styling.Stroke) =
      s match {
        case null => Nil
        case s => graphicSymbols(s.getGraphicFill) ++ graphicSymbols(s.getGraphicStroke)
      }

    def fillSymbols(f: org.geotools.styling.Fill) =
      f match {
        case null => Nil
        case f => graphicSymbols(f.getGraphicFill)
      }

    s match {
      case (p: org.geotools.styling.PointSymbolizer) => 
        graphicSymbols(p.getGraphic)
      // For KML, only points can have custom graphics anyway.
      // case (l: org.geotools.styling.LineSymbolizer) =>
      //   strokeSymbols(l.getStroke)
      // case (pl: org.geotools.styling.PolygonSymbolizer) =>
      //   fillSymbols(pl.getFill) ++ strokeSymbols(pl.getStroke)
      case _ =>
        Nil
    }
  }

}
