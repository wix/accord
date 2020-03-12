/*
  Copyright 2013-2019 Wix.com

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

package com.wix.accord.dsl

import com.wix.accord.ResultBuilders._
import com.wix.accord.{Result, Validator}

import scala.language.reflectiveCalls

// TODO ScalaDocs

trait ContextTransformer[ Inner, Outer ] {
  protected def transform: Validator[ Inner ] => Validator[ Outer ]
}

class CollectionDslContext[ Element, Coll ]( protected val transform: Validator[ Element ] => Validator[ Coll ] )
  extends DslContext[ Element, Coll ] with ContextTransformer[ Element, Coll ] {

  def apply( validator: Validator[ Int ] )( implicit ev: Element => HasSize ): Validator[ Coll ] = {
    val composed = new Validator[ Element ] {
      override def apply( v1: Element ): Result =
        if ( v1 == null )
          Validator.nullFailure
        else
          validator.boxed( v1.size ).withValue( v1 )
    }
    transform apply composed
  }
}

case class CollectionContextTransformer[ Element, Coll ]( transformWith: IndexedDescriptions[ Coll ] =>
                                                                         Validator[ Element ] =>
                                                                         Validator[ Coll ] ) {

  def apply( withIndices: IndexedDescriptions[ Coll ] ): Validator[ Element ] => Validator[ Coll ] =
    transformWith( withIndices )

  def compose[ A ]( g: Validator[ A ] => Validator[ Element ] ): CollectionContextTransformer[ A, Coll ] =
    CollectionContextTransformer[ A, Coll ](
      apply( _: IndexedDescriptions[ Coll ] ) compose g
    )
}

class CollectionEachDslContext[ Element, Coll ]( withIndices: IndexedDescriptions[ Coll ],
                                                 transformWith: CollectionContextTransformer[ Element, Coll ] )
  extends CollectionDslContext[ Element, Coll ]( transformWith( withIndices ) ) {

  /** Adapts the syntax for the collections. Enables validation rules such as `c.students.each.map(_.age) should be >= 18`.
    * @param f The function to apply to each element of a collection before subsequent validation rules are applied.
    * @tparam MappedElement The resulting element type of a collection after f is applied.
    * @return Syntax, where the collection elements are of type [[MappedElement]],
    *         resulting from applying function f to each element.
    */
  def map[ MappedElement ]( f: Element => MappedElement ) =
    new CollectionEachDslContext[ MappedElement, Coll ](
      withIndices,
      transformWith.compose { v: Validator[ MappedElement ] =>
        v compose f
      }
    )

  /** Adapts the syntax for the collections. Enables validation rules such as
    *   `c.students.each.flatMap(_.guardians).map(_.age) should be >= 30`.
    * @param f The function to apply to each element of a collection, producing a new collection after each application.
    * @tparam MappedElement The resulting element type of a collection after f is applied.
    * @return Syntax, where the collection elements are of type [[MappedElement]],
    *         resulting from applying function f to each element and concatenating the results.
    */
  def flatMap[ MappedElement ]( f: Element => Traversable[ MappedElement ] )
                              ( implicit withIndices: IndexedDescriptions[ Coll ] ) =
    new CollectionEachDslContext[ MappedElement, Coll ](
      IndexedDescriptions( includeIndices = false ),
      transformWith.compose { validator: Validator[ MappedElement ] =>
        val broadcast =
          Aggregates.all[ Traversable[ MappedElement ], MappedElement ]( withIndices.includeIndexInformation ) _
        broadcast(validator) compose f
      }
    )
}

trait IndexedDescriptions[ T ] {
  def includeIndexInformation: Boolean
}

trait FallbackIndexDescriptions {
  implicit def disableIndexDescriptionsByDefault[ T ]: IndexedDescriptions[ T ] =
    IndexedDescriptions( includeIndices = false )
}

object IndexedDescriptions extends FallbackIndexDescriptions {
  implicit def enableIndexingForSequences[ T ]( implicit ev: T => Seq[_] ): IndexedDescriptions[ T ] =
    IndexedDescriptions( includeIndices = true )

  def apply[ T ]( includeIndices: Boolean ): IndexedDescriptions[ T ] =
    new IndexedDescriptions[ T ] {
      override def includeIndexInformation: Boolean = includeIndices
    }
}

trait DslContext[ Inner, Outer ] {
  self: ContextTransformer[ Inner, Outer ] =>

  private def apply( validator: Validator[ Inner ] ): Validator[ Outer ] = transform apply validator

  def is    ( validator: Validator[ Inner ] ): Validator[ Outer ] = apply( validator )
  def should( validator: Validator[ Inner ] ): Validator[ Outer ] = apply( validator )
  def must  ( validator: Validator[ Inner ] ): Validator[ Outer ] = apply( validator )

  /** Provides extended syntax for collections; enables validation rules such as `c.students.each is valid`.
    *
    * @param ev Evidence that the provided expression can be treated as a collection.
    * @tparam Element The element type m of the specified collection.
    * @return Additional syntax (see implementation).
    */
  def each[ Element ]( implicit ev: Inner => Traversable[ Element ], withIndices: IndexedDescriptions[ Inner ] ) =
    new CollectionEachDslContext[ Element, Outer ](
      IndexedDescriptions[ Outer ]( withIndices.includeIndexInformation ),
      CollectionContextTransformer( withIndices =>
        self.transform compose Aggregates.all[ Inner, Element ]( withIndices.includeIndexInformation )
      )
    )

  private def collContext = new CollectionDslContext[ Inner, Outer ]( transform )
  def has: CollectionDslContext[ Inner, Outer ] = collContext
  def have: CollectionDslContext[ Inner, Outer ] = collContext
}

trait SimpleDslContext[ U ] extends DslContext[ U, U ] with ContextTransformer[ U, U ] {
  protected override def transform = identity
}

