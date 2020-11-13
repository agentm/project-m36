{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Newtypes for deriving Tupleable instances with customization using
-- @DerivingVia@.
--
-- Inspired by
-- [Dhall.Deriving](https://hackage.haskell.org/package/dhall-1.33.1/docs/Dhall-Deriving.html)
-- which in turn was inspired by Matt Parson's blog post
-- [Mirror Mirror: Reflection and Encoding Via](https://www.parsonsmatt.org/2020/02/04/mirror_mirror.html).
--
-- required extensions:
--
--   * DerivingVia
--   * DeriveGenerics
--   * TypeOperators (for @('<<<')@ and @('>>>')@)
--   * DataKinds (for types that take a string argument)

module ProjectM36.Tupleable.Deriving
  ( -- * DerivingVia Newtype
    Codec(..)

    -- * Type-level Options
  , ModifyOptions(..)
  , Field

    -- * Type-level 'T.Text' -> 'T.Text' Functions
  , ModifyText(..)
  , AddPrefix
  , DropPrefix
  , AddSuffix
  , DropSuffix
  , UpperCase
  , LowerCase
  , TitleCase
  , CamelCase
  , PascalCase
  , SnakeCase
  , SpinalCase
  , TrainCase

    -- * Composition
  , AsIs
  , type (<<<)
  , type (>>>)

    -- * Re-Exports
  , Generic
  , module ProjectM36.Tupleable
  ) where
import           Data.Maybe           (fromMaybe)
import           Data.Proxy
import qualified Data.Text            as T
import           Data.Text.Manipulate
import           GHC.TypeLits
import           GHC.Generics         (Generic, Rep)
import           ProjectM36.Tupleable


-- | A newtype wrapper to allow for easier deriving of 'Tupleable' instances
-- with customization.
--
-- The @tag@ type variable can be used to specify options for converting the
-- datatype to and from a 'RelationTuple'. For example,
--
-- > data Example = Example
-- >     { exampleFoo :: Int
-- >     , exampleBar :: Int
-- >     }
-- >     deriving stock (Generic)
-- >     deriving (Tupleable)
-- >         via Codec (Field (DropPrefix "example" >>> CamelCase)) Example
--
-- will derive an instance of 'Tupleable' where field names are translated into
-- attribute names by dropping the prefix @"example"@ and then converting the
-- result to camelCase. So @"exampleFoo"@ becomes @"foo"@ and @"exampleBar"@
-- becomes @"bar"@.
--
-- Requires the @DerivingGeneric@ and @DerivingVia@ extensions to be enabled.
newtype Codec tag a = Codec { unCodec :: a }

instance (ModifyOptions tag, Generic a, TupleableG (Rep a)) => Tupleable (Codec tag a) where
  toTuple v = genericToTuple opts (unCodec v)
    where
      opts = modifyOptions (Proxy :: Proxy tag) defaultTupleableOptions

  fromTuple tup = Codec <$> genericFromTuple opts tup
    where
      opts = modifyOptions (Proxy :: Proxy tag) defaultTupleableOptions

  toAttributes _ = genericToAttributes opts (Proxy :: Proxy a)
    where
      opts = modifyOptions (Proxy :: Proxy tag) defaultTupleableOptions

-- | Types that can be used as tags for 'Codec'.
class ModifyOptions a where
  modifyOptions :: proxy a -> TupleableOptions -> TupleableOptions

-- | Change how record field names are translated into attribute names. For
-- example,
--
-- > Field SnakeCase
--
-- will translate the field name @fooBar@ into the attribute name @foo_bar@.
data Field a

instance ModifyText a => ModifyOptions (Field a) where
  modifyOptions _ opts = opts { fieldModifier = newFieldModifier }
    where
      newFieldModifier = modifyText (Proxy :: Proxy a) . fieldModifier opts

-- | Types that can be used in options that modify 'T.Text' such as in 'Field'.
class ModifyText a where
  modifyText :: proxy a -> T.Text -> T.Text

-- | Add a prefix. @AddPrefix "foo"@ will transform @"bar"@ into @"foobar"@.
data AddPrefix (prefix :: Symbol)

instance KnownSymbol prefix => ModifyText (AddPrefix prefix) where
  modifyText _ oldText = prefixText <> oldText
    where
      prefixText = T.pack (symbolVal (Proxy :: Proxy prefix))

-- | Drop a prefix. @DropPrefix "bar"@ will transform @"foobar"@ into @"foo"@.
data DropPrefix (prefix :: Symbol)

instance KnownSymbol prefix => ModifyText (DropPrefix prefix) where
  modifyText _ oldText = fromMaybe oldText (T.stripPrefix prefixText oldText)
    where
      prefixText = T.pack (symbolVal (Proxy :: Proxy prefix))

-- | Add a suffix. @AddSuffix "bar"@ will transform @"foo"@ into @"foobar"@.
data AddSuffix (suffix :: Symbol)

instance KnownSymbol suffix => ModifyText (AddSuffix suffix) where
  modifyText _ oldText = oldText <> suffixText
    where
      suffixText = T.pack (symbolVal (Proxy :: Proxy suffix))

-- | Drop a suffix. @DropSuffix "bar"@ will transform @"foobar"@ into @"foo"@.
data DropSuffix (suffix :: Symbol)

instance KnownSymbol suffix => ModifyText (DropSuffix suffix) where
  modifyText _ oldText = fromMaybe oldText (T.stripSuffix suffixText oldText)
    where
      suffixText = T.pack (symbolVal (Proxy :: Proxy suffix))

-- | Convert to UPPERCASE. Will transform @"foobar"@ into @\"FOOBAR\"@.
data UpperCase

instance ModifyText UpperCase where
  modifyText _ = T.toUpper

-- | Convert to lowercase. Will transform @\"FOOBAR\"@ into @"foobar"@.
data LowerCase

instance ModifyText LowerCase where
  modifyText _ = T.toLower

-- | Convert to Title Case. Will transform @"fooBar"@ into @\"Foo Bar\"@.
data TitleCase

instance ModifyText TitleCase where
  modifyText _ = toTitle

-- | Convert to camelCase. Will transform @"foo_bar"@ into @"fooBar"@.
data CamelCase

instance ModifyText CamelCase where
  modifyText _ = toCamel

-- | Convert to PascalCase. Will transform @"foo_bar"@ into @\"FooBar\"@.
data PascalCase

instance ModifyText PascalCase where
  modifyText _ = toPascal

-- | Convert to snake_case. Will transform @"fooBar"@ into @"foo_bar"@.
data SnakeCase

instance ModifyText SnakeCase where
  modifyText _ = toSnake

-- | Convert to spinal-case. will transform @"fooBar"@ into @"foo-bar"@.
data SpinalCase

instance ModifyText SpinalCase where
  modifyText _ = toSpinal

-- | Convert to Train-Case. Will transform @"fooBar"@ into @\"Foo-Bar\"@.
data TrainCase

instance ModifyText TrainCase where
  modifyText _ = toTrain

-- | Identity option.
type AsIs = ()

instance ModifyOptions () where
  modifyOptions _ = id

instance ModifyText () where
  modifyText _ = id

-- | Right to left composition.
--
-- Requires the @TypeOperators@ extension to be enabled.
data a <<< b

instance (ModifyOptions a, ModifyOptions b) => ModifyOptions (a <<< b) where
  modifyOptions _ = modifyOptions (Proxy :: Proxy a) . modifyOptions (Proxy :: Proxy b)

instance (ModifyText a, ModifyText b) => ModifyText (a <<< b) where
  modifyText _ = modifyText (Proxy :: Proxy a) . modifyText (Proxy :: Proxy b)

-- | Left to right composition.
--
-- Requires the @TypeOperators@ extension to be enabled.
data a >>> b

instance (ModifyOptions a, ModifyOptions b) => ModifyOptions (a >>> b) where
  modifyOptions _ = modifyOptions (Proxy :: Proxy b) . modifyOptions (Proxy :: Proxy a)

instance (ModifyText a, ModifyText b) => ModifyText (a >>> b) where
  modifyText _ = modifyText (Proxy :: Proxy b) . modifyText (Proxy :: Proxy a)
