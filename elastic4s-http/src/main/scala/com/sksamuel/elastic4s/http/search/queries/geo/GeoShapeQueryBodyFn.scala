package com.sksamuel.elastic4s.http.search.queries.geo

import com.sksamuel.elastic4s.json.{XContentBuilder, XContentFactory}
import com.sksamuel.elastic4s.searches.queries.geo._

object GeoShapeQueryBodyFn {

  def apply(q: GeoShapeQueryDefinition): XContentBuilder = {

    val builder = XContentFactory.jsonBuilder()

    builder.startObject("geo_shape")
    builder.startObject(q.field)

    q.shape match {

      case InlineShape(shape: SingleShape) =>

        builder.rawField("shape", buildSingleShape(shape))

      case InlineShape(s@GeometryCollectionShape(shapes)) =>

        builder.startObject("shape")
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("geometries",shapes.map(buildSingleShape).toArray)
        builder.endObject()

      case PreindexedShape(id, index, tpe, path) =>

        builder.startObject("indexed_shape")
        builder.field("id", id)
        builder.field("index", index.name)
        builder.field("type", tpe)
        builder.field("path", path)
        builder.endObject()
    }

    q.relation.map(_.getClass.getSimpleName.toLowerCase).foreach(builder.field("relation", _))
    q.ignoreUnmapped.foreach(builder.field("ignore_unmapped", _))
    q.boost.foreach(builder.field("boost", _))
    q.queryName.foreach(builder.field("_name", _))

    builder.endObject().endObject()
  }

  private def buildSingleShape(shape: SingleShape): XContentBuilder = {
    shape match {
      case s@PointShape((x,y)) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", Array(x,y))
        builder

      case s@EnvelopeShape(coords) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", coords.map { case (a,b) => Array(a,b) }.toArray)
        builder

      case s@MultiPointShape(coords) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", coords.map { case (a,b) => Array(a,b) }.toArray)
        builder

      case s@LineStringShape(coords) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", coords.map { case (a,b) => Array(a,b) }.toArray)
        builder

      case s@MultiLineStringShape(coords) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", coords.map(_.map { case (a,b) => Array(a,b) }.toArray).toArray)
        builder

      case s@CircleShape((x,y),(radius,unit)) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        builder.array("coordinates", Array(x,y))
        builder.field("radius", unit.toMeters(radius) + "m")
        builder

      case s@PolygonShape(coordinates, holes) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        val coords = holes.fold(Seq(coordinates))(h => Seq(coordinates,h))
        builder.array("coordinates", coords.map(_.map { case (a,b) => Array(a,b) }.toArray).toArray)
        builder

      case s@MultiPolygonShape(coordinates) =>
        val builder = XContentFactory.jsonBuilder()
        builder.field("type", s.geoShapeType.toString.toLowerCase)
        val coords = coordinates.map { case (coords,holes) =>
          holes.fold(Seq(coords))(h => Seq(coords,h)).map(_.map { case (a,b) => Array(a,b) }.toArray).toArray
        }
        builder.array("coordinates",coords.toArray)
        builder
    }
  }
}
