/*
 * Copyright 2020 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fs2
package data
package json
package selector

import scala.annotation.implicitNotFound

sealed abstract class SelectorBuilder[M, S] private[selector] {

  def iterate: IteratorBuilder[Strict] =
    IteratorBuilder[Strict](true, this)

  def index(i: Int): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Single(i), true, this)

  def indices(i: Int, is: Int*): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Several(is.toSet + i), true, this)

  def range(start: Int, end: Int): IndicesBuilder[Strict] =
    IndicesBuilder[Strict](IndexPredicate.Range(start, end), true, this)

  def field(f: String): NamesBuilder[Optional, Strict] =
    NamesBuilder[Optional, Strict](NamePredicate.Single(f), true, false, this)

  def fields(f: String, fs: String*): NamesBuilder[Optional, Strict] =
    NamesBuilder[Optional, Strict](NamePredicate.Several(fs.toSet + f), true, false, this)

  def compile: Selector

}

case object RootBuilder extends SelectorBuilder[NotApplicable, NotApplicable] {
  def compile: Selector = Selector.ThisSelector
}

case class IteratorBuilder[S](strict: Boolean, parent: SelectorBuilder[_, _])
    extends SelectorBuilder[NotApplicable, S] {
  def compile: Selector = Selector.PipeSelector.from(parent.compile, Selector.IteratorSelector(strict))
}

object IteratorBuilder {

  implicit object LenientableIterator extends Lenientable[IteratorBuilder[Strict], NotApplicable] {
    def makeLenient(builder: IteratorBuilder[Strict]): SelectorBuilder[NotApplicable, Lenient] =
      builder.copy(strict = false)
  }

}

case class IndicesBuilder[S](predicate: IndexPredicate, strict: Boolean, parent: SelectorBuilder[_, _])
    extends SelectorBuilder[NotApplicable, S] {
  def compile: Selector = Selector.PipeSelector.from(parent.compile, Selector.IndexSelector(predicate, strict))
}

object IndicesBuilder {

  implicit object LenientableIndices extends Lenientable[IndicesBuilder[Strict], NotApplicable] {
    def makeLenient(builder: IndicesBuilder[Strict]): SelectorBuilder[NotApplicable, Lenient] =
      builder.copy(strict = false)
  }

}

case class NamesBuilder[M, S](predicate: NamePredicate,
                              strict: Boolean,
                              mandatory: Boolean,
                              parent: SelectorBuilder[_, _])
    extends SelectorBuilder[M, S] {
  def compile: Selector = Selector.PipeSelector.from(parent.compile, Selector.NameSelector(predicate, strict, mandatory))
}

object NamesBuilder {

  implicit def LenientableNames[M]: Lenientable[NamesBuilder[M, Strict], M] =
    new Lenientable[NamesBuilder[M, Strict], M] {
      def makeLenient(builder: NamesBuilder[M, Strict]): SelectorBuilder[M, Lenient] =
        builder.copy(strict = false)
    }

  implicit def MandatoriableNames[S]: Mandatoriable[NamesBuilder[Optional, S], S] =
    new Mandatoriable[NamesBuilder[Optional, S], S] {
      def makeMandatory(builder: NamesBuilder[Optional, S]): SelectorBuilder[Mandatory, S] =
        builder.copy(mandatory = true)
    }

}

class NotApplicable private {}
class Optional private {}
class Mandatory private {}
class Strict private {}
class Lenient private {}

@implicitNotFound(msg = "There seems to be no way to make ${Builder} lenient")
sealed trait Lenientable[Builder, M] {
  def makeLenient(builder: Builder): SelectorBuilder[M, Lenient]
}

@implicitNotFound(msg = "Only field selectors can be made mandatory (not ${Builder})")
sealed trait Mandatoriable[Builder, S] {
  def makeMandatory(builder: Builder): SelectorBuilder[Mandatory, S]
}

object SelectorBuilder {

  implicit class SelectorOps[M, S, B <: SelectorBuilder[M, S]](val inner: B with SelectorBuilder[M, S]) extends AnyVal {

    def !(implicit M: Mandatoriable[B, S]): SelectorBuilder[Mandatory, S] =
      M.makeMandatory(inner)

    def ?(implicit L: Lenientable[B, M]): SelectorBuilder[M, Lenient] =
      L.makeLenient(inner)

  }

}
