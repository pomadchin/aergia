package com.azavea.dsl

import cats.Functor
import cats.free.{Cofree, Coyoneda}
import higherkindness.droste.data.Fix
import higherkindness.droste.syntax.fix._
import higherkindness.droste.{Algebra, scheme}
import geotrellis.raster._
import geotrellis.vector.Extent

import java.net.URI

sealed trait ExpressionF[A]

object ExpressionF {
  type Expression = Fix[ExpressionF]

  case class Addition[A](l: A, r: A)                      extends ExpressionF[A]
  case class Substraction[A](l: A, r: A)                  extends ExpressionF[A]
  case class RasterRefRegion[A](uri: URI, extent: Extent) extends ExpressionF[A]
  case class RasterRef[A](uri: URI)                       extends ExpressionF[A]

  def addition(l: Expression, r: Expression): Expression     = Addition(l, r).fix
  def substraction(l: Expression, r: Expression): Expression = Substraction(l, r).fix
  def rasterRefRegion(uri: URI, extent: Extent): Expression  = RasterRefRegion(uri, extent).fix
  def rasterRef(uri: URI): Expression                        = RasterRef(uri).fix

  implicit val expressionFFunctor: Functor[ExpressionF] =
    new Functor[ExpressionF] {
      def map[A, B](fa: ExpressionF[A])(f: A => B): ExpressionF[B] = {
        fa match {
          case Addition(l, r)               => Addition(f(l), f(r))
          case Substraction(l, r)           => Substraction(f(l), f(r))
          case RasterRefRegion(uri, extent) => RasterRefRegion[B](uri, extent)
          case RasterRef(uri)               => RasterRef[B](uri)
        }
      }
    }

  // should be lazy tile (?) how to lift it to make lazy?
  type LazyTile = () => Tile
  // substraction alreay has a loaded tile
  // how to dealy this up to the very end?
  // I want stac operations and apply them // ?
  // should this be just a Tile => Tile algebra?
  // how about coyoneda? 
  // rec scheme a cofree structure
  // https://pdf.sciencedirectassets.com/272990/1-s2.0-S1571066111X00063/1-s2.0-S1571066111000570/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEEMaCXVzLWVhc3QtMSJGMEQCIBgXRJb7N%2BwXvu8Qn6pBVtbMagrMvW2igW26hVRavOxSAiBJByzg2BMYqvU7byzQgu15r2ev1vpXtAIIFZ3E2BGx5iq9AwjL%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F8BEAMaDDA1OTAwMzU0Njg2NSIMwKGCgeh%2BtvJJDMAuKpEDdoOh3WM3EKL6LmjdEaCFBtI9oG6SYL0QymMOuVwOA7djl1%2FtNbKB79yKpt6OZJjq%2Byw4iHwNmEXirX%2B0gvDHGjhNButG%2FjOaaZpb1CPPDXkXA8iBiLOiItrl3yhtmGsOpXRo7Z43ePp6VZjXDh9ICij1NLCAfq481oV6Ar3lTIZWzAH4bIXr7M4Dlpa53R45vw1LWa%2B89hRYkkhDAey0RPb6L%2F3px3Ngb6jMDPfDHCe8XAe9fY5aS1ulXOa83URQQHAnQmCSrARUd9ZV1Zkwqj8sUvqEDgdyXF5o415136zoaT67wv9PpsAUsI2DKRu0zpFApjSvbOtP%2B7A3C4Y7soXvMWaNKb6rX2R9PzjaGTgrqMuKcwUyAtmT8xoSII9gEeuR24Rycw%2BYofyBZrQyQ7Zha2EqTPemKuzGiX7kAiVyvBaKEZ1IKBDo1C9sIErfW8AeVK0hCuKW9o4WLskAwFHIF0Ntc8t7u3DJnnMpxJG5L%2BuwfvQbXluITcTCDSBJoSXlcqP7foU7em9E9%2BhumjAw4bim%2FgU67AFZiItWMX75lrLICVI7tRbK1Dx%2FLfhJT%2Bx25ursXkNZxai9gf73HJdA%2FQ8s19fiZ5ZSUVrPBaX356tqNQMPaQZ3ruLO1mjJT8yQK%2BjYw9rQPB%2FLOVrFyd13RfqFAvHo%2BDcO9IfAqYRgcw%2BVhgwiKRiNrZY0VZVVRDeFyGVESCtZwwEcLi1Mrc28VJEi8WWvsNIHHuXADeP7PCdcNswj4Hf0pDx%2F0kj26RiE3WNCjhYAquCYdjuCjBqlQskB39qqEqEYxg9758Q4rVQ%2FGFamvANoFWCLETrMJpszwZRjWEf6IsJ92DqKAqi0ajeJAw%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20201204T031352Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYZKKMHCLO%2F20201204%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=31c203872163d793287e6c2b4d57e0050151f0f6e749c941a48ac164bb185032&hash=893caf24685de4c6139a890f8b46716061d159023214ef79279d4cb7286954cf&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1571066111000570&tid=spdf-38b92c22-0046-42a0-9faa-43b35b5ce624&sid=3bf25f876631094d017b60f35115a5a20ed1gxrqa&type=client
  // https://github.com/softwaremill/recursion-training
  // https://kubuszok.com/2019/ast-playground-recursion-schemes-and-recursive-data/
  // https://medium.com/@olxc/yoneda-and-coyoneda-trick-f5a0321aeba4
  // http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/
  // https://blog.oyanglul.us/grokking-monad/scala/en/part3
  // https://github.com/typelevel/cats/search?q=CoYoneda
  // https://arxiv.org/pdf/2010.06366.pdf
  // optimizations https://github.com/softwaremill/recursion-training#transforming-recursive-expressions
  def algebra: Algebra[ExpressionF, Tile] = Algebra {
    case RasterRefRegion(uri, extent) => RasterSource(uri.toString).read(extent).get.tile.band(0)
    case RasterRef(uri)               =>
        println(s"---- RasterRef(${uri})")
        RasterSource(uri.toString).read().get.tile.band(0)
    case Substraction(l, r)           =>
        println(s"---- Substraction${(l, r)}")
        l - r
    case Addition(l, r)               =>
        println(s"---- Addition${(l, r)}")
        l + r
  }

  def algebraDT: Algebra[ExpressionF, DelegatingTile2] = Algebra {
    case RasterRefRegion(uri, extent) =>
      DelegatingTile2.fromRasterSource(RasterSource(uri.toString), extent)
    case RasterRef(uri)               =>
      println(s"---- RasterRef(${uri})")
      DelegatingTile2.fromRasterSource(RasterSource(uri.toString))
    case Substraction(l, r)           =>
      println(s"---- Substraction${(l, r)}")
      new DelegatingTile2 {
        protected def delegate: Tile = {
          println("substraction delegate invoked")
          l - r
        }
        override def cols: Int = l.cols
        override def rows: Int = l.rows
        override def cellType: CellType = l.cellType
      }
    case Addition(l, r)               =>
      println(s"---- Addition${(l, r)}")
      new DelegatingTile2 {
        protected def delegate: Tile = {
          println("addition delegate invoked")
          l + r
        }
        override def cols: Int = l.cols
        override def rows: Int = l.rows
        override def cellType: CellType = l.cellType
      }
  }

  /*case class CYTile[A](tile: A)

  implicit val CYTileFunctor: Functor[CYTile] =
    new Functor[CYTile] {
      def map[A, B](fa: CYTile[A])(f: A => B): CYTile[B] = {
        case CYTile(a) => CYTile(f(a))
      }
    }*/

 /*  def algebraCY: Algebra[ExpressionF, Coyoneda[CYTile, Tile]] = Algebra {
    case RasterRefRegion(uri, extent) =>
      Coyoneda(CYTile(RasterSource(uri.toString).read(extent).get.tile.band(0)))(identity)
    case RasterRef(uri)               =>
      println(s"---- RasterRef(${uri})")
      Coyoneda(CYTile(RasterSource(uri.toString).read().get.tile.band(0)))(identity)
    case Substraction(l, r)           =>
      println(s"---- Substraction${(l, r)}")
      l.map(tile => tile - r(tile))
    case Addition(l, r)               =>
      println(s"---- Addition${(l, r)}")
      l + r
  } */


  /** An alias for [[scheme.cata]] since it can confuse people */
  def eval(expression: Expression): Tile =
    scheme.cata(algebra).apply(expression)

  def evalDT(expression: Expression): DelegatingTile2 =
    scheme.cata(algebraDT).apply(expression)

}
