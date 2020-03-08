package com.ming

import cats.effect._
import org.scalatest._
import com.ming.Demo._
import com.ming.Http.Method.GET
import com.ming.Http.Status.{NotFound, OK}
import com.ming.Http._

class DemoSpec extends FlatSpec with Matchers {


  "App" should "return hello" in {
    val request = Request(GET, Uri("/hello"), "")
    val expectedResponse = Response(OK, "hello")
    val app: HttpApp[IO] = Demo.routes[IO]
    val ioResponse: IO[Response] = app(request)
    val response: Response = ioResponse.unsafeRunSync()

    response shouldBe expectedResponse
  }

  "App" should "return status" in {
    val request = Request(GET, Uri("/status"), "")
    val expectedResponse = Response(OK, "OK")
    val app: HttpApp[IO] = Demo.routes[IO]
    val ioResponse: IO[Response] = app(request)
    val response: Response = ioResponse.unsafeRunSync()

    response shouldBe expectedResponse
  }

  "App" should "return NotFound" in {
    val request = Request(GET, Uri("/invalid"), "")
    val expectedResponse = Response(NotFound, "")
    val app: HttpApp[IO] = Demo.routes[IO]
    val ioResponse: IO[Response] = app(request)
    val response: Response = ioResponse.unsafeRunSync()

    response shouldBe expectedResponse
  }

}
