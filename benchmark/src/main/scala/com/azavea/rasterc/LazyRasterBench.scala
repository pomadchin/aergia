package com.azavea.rasterc

import geotrellis.raster._
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Benchmark)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class LazyRasterBench {
  import LazyTile._

  /**
   * jmh:run -i 10 -wi 5 -f1 -t1 com.azavea.rasterc.LazyRasterBench
   *
   * [info] # JMH version: 1.25
   * [info] # VM version: JDK 11.0.9, OpenJDK 64-Bit Server VM, 11.0.9+11
   * [info] # VM options: -Xmx4G
   * [info] Benchmark                                  Mode  Cnt     Score     Error  Units
   * [info] LazyRasterBench.LazyTileIntNumberAdd50000  avgt   10  1010.776 ± 226.610  ms/op
   * [info] LazyRasterBench.LazyTileIntTileAdd50000    avgt   10  1471.099 ± 277.511  ms/op
   * [info] LazyRasterBench.TileIntNumberAdd50000      avgt   10    10.895 ±   0.611  ms/op
   * [info] LazyRasterBench.TileIntTileAdd50000        avgt   10    17.021 ±   0.654  ms/op
   */

  val tile0: Tile = IntArrayTile.fill(0, 10, 10)
  val ltile0: LazyTile = LazyTile.fromTile(tile0)

  @Benchmark
  def LazyTileIntNumberAdd50000(): Tile =
    (1 to 50000).foldLeft(ltile0)((e, acc) => e + acc).toArrayTile

  @Benchmark
  def LazyTileIntTileAdd50000(): Tile =
    (1 to 50000).foldLeft(ltile0)((e, acc) => e + LazyTile.fromTile(IntArrayTile.fill(acc, 10, 10))).toArrayTile

  @Benchmark
  def TileIntNumberAdd50000(): Tile =
    (1 to 50000).foldLeft(tile0)((e, acc) => e + acc).toArrayTile

  @Benchmark
  def TileIntTileAdd50000(): Tile =
    (1 to 50000).foldLeft(tile0)((e, acc) => e + IntArrayTile.fill(acc, 10, 10)).toArrayTile

}