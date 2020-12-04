package com.azavea.dsl

import geotrellis.raster._
import geotrellis.vector.Extent

/**
 * A tile that wraps another tile. Originally intended for delayed reading, but useful in other special use cases.
 *
 * @since 8/22/18
 */
abstract class DelegatingTile2 extends Tile {
  protected def delegate: Tile

  def div = delegate / delegate

  def cellType: CellType =
    delegate.cellType

  def cols: Int =
    delegate.cols

  def rows: Int =
    delegate.rows

  def mutable: MutableArrayTile =
    delegate.mutable

  def convert(cellType: CellType): Tile =
    delegate.convert(cellType)

  override def withNoData(noDataValue: Option[Double]): Tile =
    delegate.withNoData(noDataValue)

  def interpretAs(newCellType: CellType): Tile =
    delegate.interpretAs(newCellType)

  def get(col: Int, row: Int): Int =
    delegate.get(col, row)

  def getDouble(col: Int, row: Int): Double =
    delegate.getDouble(col, row)

  def toArrayTile(): ArrayTile =
    delegate.toArrayTile()

  def toArray(): Array[Int] =
    delegate.toArray()

  def toArrayDouble(): Array[Double] =
    delegate.toArrayDouble()

  def toBytes(): Array[Byte] =
    delegate.toBytes()

  def foreach(f: Int ⇒ Unit): Unit =
    delegate.foreach(f)

  def foreachDouble(f: Double ⇒ Unit): Unit =
    delegate.foreachDouble(f)

  def map(f: Int ⇒ Int): Tile =
    delegate.map(f)

  def combine(r2: Tile)(f: (Int, Int) ⇒ Int): Tile =
    delegate.combine(r2)(f)

  def mapDouble(f: Double ⇒ Double): Tile =
    delegate.mapDouble(f)

  def combineDouble(r2: Tile)(f: (Double, Double) ⇒ Double): Tile =
    delegate.combineDouble(r2)(f)

  def foreachIntVisitor(visitor: IntTileVisitor): Unit =
    delegate.foreachIntVisitor(visitor)

  def foreachDoubleVisitor(visitor: DoubleTileVisitor): Unit =
    delegate.foreachDoubleVisitor(visitor)

  def mapIntMapper(mapper: IntTileMapper): Tile =
    delegate.mapIntMapper(mapper)

  def mapDoubleMapper(mapper: DoubleTileMapper): Tile =
    delegate.mapDoubleMapper(mapper)

  override def toString: String = s"DelegatingTile2($cols, $rows, $cellType)"
}

object DelegatingTile2 {
  def fromRasterSource(rs: RasterSource, extent: Extent): DelegatingTile2 = new DelegatingTile2 {
    protected def delegate: Tile = {
      println("I'm invoked")
      rs.read(extent).get.tile.band(0)
    }
    override def cols: Int = rs.cols.toInt
    override def rows: Int = rs.rows.toInt
    override def cellType: CellType = rs.cellType
  }

  def fromRasterSource(rs: RasterSource): DelegatingTile2 = new DelegatingTile2 {
    protected def delegate: Tile = {
      println("I'm invoked")
      rs.read().get.tile.band(0)
    }
    override def cols: Int = rs.cols.toInt
    override def rows: Int = rs.rows.toInt
    override def cellType: CellType = rs.cellType
  }
}
