package com.ming

object Http {
  type HttpRoutes[F[_]] = PartialFunction[Request, F[Response]]
  type HttpApp[F[_]] = Request => F[Response]

  object HttpRoutes {
    import cats._
    import Http.Status.NotFound

    implicit class httpRoutesSyntax[F[_]](route: HttpRoutes[F]) {
      def of(pf: PartialFunction[Request, F[Response]]): HttpRoutes[F] = pf
      def ~>(next: HttpRoutes[F]): HttpRoutes[F] = route.orElse(next)
    }

    def seal[F[_]](routes: HttpRoutes[F])(implicit ap: Applicative[F]): HttpApp[F] = req => routes.lift(req) match {
      case Some(r) => r
      case _ => ap.pure(Response(NotFound, ""))
    }
  }

  case class Request(method: Method, uri: Uri, body: String)
  case class Response(status: Status, body: String)
  sealed class Status(val code: Int)
  object Status {
    case object OK extends Status(200)
    case object NotFound extends Status(404)
  }

  case class Uri(value: String) extends AnyVal
  sealed trait Method
  object Method {
    case object GET extends Method
    case object POST extends Method
    case object PUT extends Method
    case object DELETE extends Method
  }
}

object Demo extends App {
  import cats.Applicative
  import Http._
  import Http.Uri
  import Http.Status._
  import Http.HttpRoutes._

  def hello[F[_]: Applicative]: HttpRoutes[F] = {
    case Request(method, Uri("/hello"), body) => Applicative[F].pure(Response(OK, "hello"))
  }

  def status[F[_]: Applicative]: HttpRoutes[F] = {
    case Request(method, Uri("/status"), body) => Applicative[F].pure(Response(OK, "OK"))
  }

  def routes[F[_]: Applicative]: HttpApp[F] = HttpRoutes.seal[F](hello ~> status)

}

