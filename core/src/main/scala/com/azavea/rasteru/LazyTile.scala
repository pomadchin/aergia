package com.azavea.rasteru

import cats.{Eval, Semigroup}
import com.azavea.rasteru.LazyTile.unsafeApply
import geotrellis.raster._
import spire.implicits.cfor

trait LazyTile { self =>
  @inline def cellType: CellType
  @inline def cols: Int
  @inline def rows: Int
  @inline def get(col: Int, row: Int): Int = ki(getRaw(col, row))
  @inline def getDouble(col: Int, row: Int): Double = kd(getDoubleRaw(col, row))

  val getRaw: (Int, Int) => Int
  val getDoubleRaw: (Int, Int) => Double

  /**
   * trick from [[cats.free.Coyoneda]] gives x10 performance improvement comp to Eval solution
   * */
  def ksi: List[Int => Int]
  def ksd: List[Double => Double]

  @inline final def ki: Int => Int = Function.chain(ksi.reverse)(_)
  @inline final def kd: Double => Double = Function.chain(ksd.reverse)(_)

  def toArrayTile: ArrayTile = {
    val result = ArrayTile.alloc(cellType, cols, rows)
    cfor(0)(_ < cols, _ + 1) { c =>
      cfor(0)(_ < rows, _ + 1) { r =>
        result.setDouble(r, c, getDouble(c, r))
      }
    }

    result
  }

  @inline final def dualMap(fi: Int => Int, fd: Double => Double): LazyTile =
    unsafeApply(cellType, cols, rows)(getRaw, getDoubleRaw)(fi :: ksi, fd :: ksd)
}

object LazyTile {
  def unsafeApply(ct: CellType, cls: Int, rws: Int)
                 (getIntRawF: (Int, Int) => Int, getDoubleRawF: (Int, Int) => Double)
                 (ks0: List[Int => Int], ks1: List[Double => Double]): LazyTile =
    new LazyTile {
      val ksi: List[Int => Int] = ks0
      val ksd: List[Double => Double] = ks1
      val cellType: CellType = ct
      val cols: Int = cls
      val rows: Int = rws
      val getRaw: (Int, Int) => Int = (col, row) => getIntRawF(col, row)
      val getDoubleRaw: (Int, Int) => Double = (col, row) => getDoubleRawF(col, row)
    }

  def fromTile(tile: Tile): LazyTile = new LazyTile {
    val ksi: List[Int => Int] = Nil
    val ksd: List[Double => Double] = Nil
    val cellType: CellType = tile.cellType
    val cols: Int = tile.cols
    val rows: Int = tile.rows
    val getRaw: (Int, Int) => Int = (col, row) => tile.get(col, row)
    val getDoubleRaw: (Int, Int) => Double = (col, row) => tile.get(col, row)
  }

  def semigroup(fis: (Int, Int) => Int, fds: (Double, Double) => Double): Semigroup[LazyTile] = {
    (l, r) => new LazyTile {
      val ksi: List[Int => Int] = Nil
      val ksd: List[Double => Double] = Nil
      val cellType: CellType = l.cellType
      val cols: Int = l.cols
      val rows: Int = l.rows
      val getRaw: (Int, Int) => Int = ???
      val getDoubleRaw: (Int, Int) => Double = ???
    }
  }

  val semigroupAdd: Semigroup[LazyTile] = semigroup(
    { (z1, z2) => if (isNoData(z1) || isNoData(z2)) NODATA else z1 + z2 },
    { (z1, z2) => if (isNoData(z1) || isNoData(z2)) Double.NaN else z1 + z2 }
  )

  implicit class LazyTileOps(self: LazyTile) {
    def localAdd(i: Int): LazyTile = self.dualMap(
      { z => if (isNoData(z) || isNoData(i)) NODATA else z + i },
      { z =>
        val d = i2d(i)
        if (isNoData(z) || isNoData(d)) Double.NaN else z + d
      })

    def +(i: Int): LazyTile = localAdd(i)

    def +:(i: Int): LazyTile = localAdd(i)

    def localAdd(d: Double): LazyTile = self.dualMap(
      { z =>
        val i = d2i(d)
        if (isNoData(z) || isNoData(i)) NODATA else z + i },
      { z =>
        if (isNoData(z) || isNoData(d)) Double.NaN else z + d
      })

    def +(d: Double): LazyTile = localAdd(d)

    def +:(d: Double): LazyTile = localAdd(d)

    def localAdd(r: LazyTile): LazyTile = semigroupAdd.combine(self, r)
    /** Add the values of each cell in each raster. */
    def +(r: LazyTile): LazyTile = localAdd(r)
    /** Add the values of each cell in each raster.  */
    def localAdd(rs: Traversable[LazyTile]): LazyTile = rs.reduce(semigroupAdd.combine)
    /** Add the values of each cell in each raster. */
    def +(rs: Traversable[LazyTile]): LazyTile = localAdd(rs)
  }
}

object LazyTest {
  def timedCreate[T](f: => T): T = {
      val s = System.currentTimeMillis
      val result = f
      val e = System.currentTimeMillis
      val t = "%,d".format(e - s)
      println(s"(in $t ms)")

      result

  }

  import LazyTile._
  def main(args: Array[String]): Unit ={
    val tile0: Tile = IntArrayTile.fill(0, 10, 10)
    val tile2: Tile = IntArrayTile.fill(2, 10, 10)
    val tile3: Tile = IntArrayTile.fill(3, 10, 10)

    val ltile0: LazyTile = LazyTile.fromTile(tile0)
    val ltile2: LazyTile = LazyTile.fromTile(tile2)
    val ltile3: LazyTile = LazyTile.fromTile(tile3)

    val rr5k = timedCreate((1 to 5000).foldLeft(ltile0)((e, acc) => e + acc))
    // println(rr5k.getDoubleRaw)
    println(timedCreate(rr5k.toArrayTile).asciiDraw)

    println("------------")

    // val rr5k2 = timedCreate((1 to 5000).foldLeft(ltile0)((e, acc) => e + LazyTile.fromTile(IntArrayTile.fill(acc, 10, 10))))
    // println(rr5k2.getDoubleRaw)
    // println(timedCreate(rr5k2.toArrayTile).asciiDraw)

    println("------------")

    val rr5ke = timedCreate((1 to 5000).foldLeft(tile0)((e, acc) => e + acc))
    println(timedCreate(rr5ke.toArrayTile).asciiDraw)

    // val rr = ((ltile0 + ltile2) + ltile3).toArrayTile

    // println(rr.asciiDraw())
    // println(rr.cellType)

    // Thread.sleep(100000)

  }
}