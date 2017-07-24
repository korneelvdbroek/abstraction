package com.fbot.main

import com.fbot.algos.nearestneighbors.NearestNeighbors
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.hyperspace.{HyperSpace, Tuple}

/**
  *
  */
case class PointCloud(space: HyperSpace, points: ImmutableArray[Tuple]) extends NearestNeighbors