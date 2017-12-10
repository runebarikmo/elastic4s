package com.sksamuel.elastic4s.http.search.queries.geo

import com.sksamuel.elastic4s.DistanceUnit
import com.sksamuel.elastic4s.searches.queries.geo._
import org.scalatest.{FunSuite, GivenWhenThen, Matchers}

class GeoShapeQueryBodyFnTest extends FunSuite with Matchers with GivenWhenThen {

  test("Should correctly build geo shape point search query") {
    Given("Some point query")
    val query = GeoShapeQueryDefinition(
      "location",
      InlineShape(
        PointShape((23.23,100.23))
      )
    )

    When("Geo shape query is built")
    val queryBody = GeoShapeQueryBodyFn(query)

    Then("Should have right field and coordinate")
    queryBody.string() shouldEqual pointQuery
  }

  test("Should correctly build geo shape circle search query") {
    Given("Some circle query")
    val query = GeoShapeQueryDefinition(
      "location",
      InlineShape(
        CircleShape((23.23,100.23),(100.0,DistanceUnit.Meters))
      )
    )

    When("Geo shape query is built")
    val queryBody = GeoShapeQueryBodyFn(query)

    Then("Should have right field, coordinates and radius")
    queryBody.string() shouldEqual circleQuery
  }

  test("Should correctly build geo shape geometry collection search query") {
    Given("Some collection shape query")
    val query = GeoShapeQueryDefinition(
      "location",
      InlineShape(
        GeometryCollectionShape(
          Seq(
            CircleShape((23.23,100.23),(100.0,DistanceUnit.Meters)),
            PointShape((23.23,100.23))
          )
        )
      )
    )

    When("Geo shape query is built")
    val queryBody = GeoShapeQueryBodyFn(query)

    Then("Should have all shapes in collection specified")
    queryBody.string() shouldEqual geometryCollectionQuery
  }

  test("Should correctly build geo shape polygon search query") {
    Given("Some polygon shape query")
    val query = GeoShapeQueryDefinition(
      "location",
      InlineShape(
        PolygonShape(
          coordinates = Seq((100.0, 0.0),(101.0, 0.0),(101.0, 1.0),(100.0, 1.0),(100.0, 0.0)),
          holes = Some(
            Seq((100.2, 0.2), (100.8, 0.2), (100.8, 0.8), (100.2, 0.8), (100.2, 0.2))
          )
        )
      )
    )


    When("Geo shape query is built")
    val queryBody = GeoShapeQueryBodyFn(query)

    Then("Should have correct coordinates")
    queryBody.string() shouldEqual polygonQuery
  }


  def polygonQuery =
  """
    |{
    |   "geo_shape":{
    |      "location":{
    |         "shape":{
    |            "type":"polygon",
    |            "coordinates":[
    |               [[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0],[100.0,0.0]],
    |               [[100.2,0.2],[100.8,0.2],[100.8,0.8],[100.2,0.8],[100.2,0.2]]
    |            ]
    |         }
    |      }
    |   }
    |}
  """.stripMargin.replaceAllLiterally(" ", "").replace("\n", "")

  def pointQuery =
  """
    |{
    |   "geo_shape":{
    |      "location":{
    |         "shape":{
    |            "type":"point",
    |            "coordinates":[
    |               23.23,
    |               100.23
    |            ]
    |         }
    |      }
    |   }
    |}
  """.stripMargin.replaceAllLiterally(" ", "").replace("\n", "")

  def circleQuery =
  """
    |{
    |   "geo_shape":{
    |      "location":{
    |         "shape":{
    |            "type":"circle",
    |            "coordinates":[
    |               23.23,
    |               100.23
    |            ],
    |            "radius":"100.0m"
    |         }
    |      }
    |   }
    |}
  """.stripMargin.replaceAllLiterally(" ", "").replace("\n", "")

  def geometryCollectionQuery =
  """
    |{
    |   "geo_shape":{
    |      "location":{
    |         "shape":{
    |            "type":"geometrycollection",
    |            "geometries":[
    |               {
    |                  "type":"circle",
    |                  "coordinates":[
    |                     23.23,
    |                     100.23
    |                  ],
    |                  "radius":"100.0m"
    |               },
    |               {
    |                  "type":"point",
    |                  "coordinates":[
    |                     23.23,
    |                     100.23
    |                  ]
    |               }
    |            ]
    |         }
    |      }
    |   }
    |}
  """.stripMargin.replaceAllLiterally(" ", "").replace("\n", "")
}


