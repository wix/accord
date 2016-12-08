/*
  Copyright 2013-2016 Wix.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 */

package com.wix.accord.java8


import com.wix.accord.java8._
import org.scalatest.{Matchers, WordSpec}

import TemporalOpsTests._
class TemporalOpsTests extends WordSpec with Matchers {

  "The expression \"is before\"" should {
    "produce a Before combinator" in {
      beforeValidator shouldBe a[ Before[_] ]
    }
  }

  "The expression \"is after\"" should {
    "produce an After combinator" in {
      afterValidator shouldBe an[ After[_] ]
    }
  }
}

object TemporalOpsTests {
  import com.wix.accord.dsl._
  import java.time.LocalDateTime
  import java.time.temporal.{ChronoUnit, Temporal}

  val now: Temporal = LocalDateTime.now()
  val lastWeek: Temporal = now.minus( 1L, ChronoUnit.WEEKS )
  val tomorrow: Temporal = now.plus( 1L, ChronoUnit.DAYS )

  val beforeValidator = lastWeek is before( now )
  val afterValidator = tomorrow is after( now )
//  val withinTimeUnitValidator = tomorrow is within( 1L, ChronoUnit.WEEKS ) of now
//  val withinDurationValidator = tomorrow is within( Duration.ofDays( 7L ) ) of now
}