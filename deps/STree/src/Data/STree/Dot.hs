{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Rendering a tree as a Graphviz graph, for inspecting the layout.
--
-- > writeFile "tree.dot" (toDot tree)
-- > -- dot -Tsvg tree.dot -o tree.svg
--
-- Each node is drawn as a record of its @L@ keys, with the edges leaving from
-- between them, so the fanout and the key ranges line up the way a B-tree is
-- normally drawn. Node names are the keys' offset into the layout array, which
-- is what makes this useful for checking the permutation rather than merely
-- looking at it: @n0@ is the root, and a node named @n34@ starts at
-- @layout ! 34@.
--
-- The deepest level is partial, so its last node can hold fewer than @L@ keys;
-- the sentinel padding that fills it out is drawn as @~@. Dummy children — the
-- ones the descent skips because their offset is past the end of the data —
-- are not drawn at all, which is why some ports have no edge.
--
-- == Showing the array as well
--
-- With @'dotArray' = True@ the graph also carries the flat array the tree lives
-- in, as a row of cells beneath it, and both are shaded by level:
--
-- > toDotStyled dotStyle { dotArray = True } tree
--
-- Because level @d@ occupies a contiguous block starting at @B^d - 1@, the array
-- comes out as one band of colour per level, matching the shading of the nodes
-- above. That correspondence is the whole point of the pairing, and it needs no
-- connecting edges — which is just as well, since one edge per node would bury
-- the picture. Every cell is also labelled with its index, so the encoding does
-- not depend on colour alone.
--
-- This is a debugging and teaching aid. A tree of any real size produces a graph
-- far too large for Graphviz to lay out usefully; keep it to a few hundred keys.
module Data.STree.Dot
  ( toDot
  , toDotStyled
  , DotStyle (..)
  , dotStyle

    -- * The map variant
  , toDotBPlus
  ) where

import Data.List (intercalate)
import qualified Data.Vector.Unboxed as U

import Data.STree.BPlusTree (BPlusTree)
import qualified Data.STree.BPlusTree as BP
import Data.STree.BTree
import Data.STree.Key (Key)

-- | How to render a tree.
data DotStyle a = DotStyle
  { dotKey :: a -> String
  -- ^ How to print one key. 'show' unless the output is too long to read in a
  -- diagram.
  , dotArray :: Bool
  -- ^ Also draw the flat layout array, shaded by level, beneath the tree.
  }

-- | 'show' for the keys, tree only.
dotStyle :: Show a => DotStyle a
dotStyle = DotStyle {dotKey = show, dotArray = False}

-- | Render with the defaults.
toDot :: forall l a. (LineWidth l, U.Unbox a, Show a) => BTree l a -> String
toDot = toDotStyled dotStyle

toDotStyled :: forall l a. (LineWidth l, U.Unbox a) => DotStyle a -> BTree l a -> String
toDotStyled style t =
  unlines $
    [ "digraph STree {"
    , "  graph [rankdir=TB, ordering=out, labelloc=\"t\"];"
    , "  label=" ++ quoted summary ++ ";"
    , "  node [shape=record, fontname=\"monospace\", fontsize=10, style=filled];"
    , "  edge [arrowsize=0.6];"
    ]
      ++ body
      ++ (if dotArray style && n > 0 then arraySection else [])
      ++ ["}"]
  where
    render = dotKey style
    l = lineWidth @l
    b = l + 1
    n = size t
    h = height t
    arr = layout t

    summary =
      "S-tree: "
        ++ show n
        ++ " keys, L="
        ++ show l
        ++ " (fanout "
        ++ show b
        ++ "), height "
        ++ show h

    body
      | n == 0 = ["  // empty tree"]
      | otherwise = renderNodes "n" render t

    -- ---------------------------------------------------------------------
    -- The array, and a legend for the shading
    -- ---------------------------------------------------------------------

    -- Record labels take one fill per node, so per-cell colour needs an
    -- HTML-like label instead.
    arraySection =
      [ "  layoutArray [shape=plaintext, fontname=\"monospace\", label=<" ++ arrayTable ++ ">];"
      , "  legend [shape=plaintext, fontname=\"monospace\", label=<" ++ legendTable ++ ">];"
      , -- rank=sink alone does not work: with no edge to the tree the array is a
        -- separate component, and dot packs those side by side. One invisible
        -- edge puts it a rank below instead, without the layout distortion an
        -- edge per leaf would cause. Anchoring it to the leftmost node of the
        -- deepest level rather than the root keeps the wide array roughly under
        -- the tree instead of hanging off to the right.
        "  n" ++ show (leftmostDeepest t) ++ " -> layoutArray [style=invis];"
      , "  { rank=same; layoutArray; legend; }"
      ]

    total = U.length arr

    arrayTable =
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"3\">"
        ++ "<TR>"
        ++ concatMap keyCell [0 .. total - 1]
        ++ "</TR><TR>"
        ++ concatMap idxCell [0 .. total - 1]
        ++ "</TR></TABLE>"
      where
        keyCell i
          | i < n =
              "<TD BGCOLOR=\"" ++ levelColour (levelOf i) ++ "\">"
                ++ escapeHtml (render (U.unsafeIndex arr i))
                ++ "</TD>"
          | otherwise = "<TD BGCOLOR=\"#e0e0e0\">~</TD>"
        idxCell i =
          "<TD BORDER=\"0\"><FONT POINT-SIZE=\"8\">" ++ show i ++ "</FONT></TD>"

    legendTable =
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"3\">"
        ++ "<TR><TD COLSPAN=\"2\" BORDER=\"0\"><B>levels</B></TD></TR>"
        ++ concatMap row [0 .. h - 1]
        ++ "</TABLE>"
      where
        row d =
          "<TR><TD BGCOLOR=\"" ++ levelColour d ++ "\">" ++ show d ++ "</TD>"
            ++ "<TD BORDER=\"0\"><FONT POINT-SIZE=\"9\">from "
            ++ show (pow d - 1)
            ++ ", "
            ++ show (nodesAt d)
            ++ " node(s)</FONT></TD></TR>"
        -- Every level but the deepest is full; the deepest holds what is left.
        nodesAt d
          | d < h - 1 = pow d
          | otherwise = (total - (pow d - 1) + l - 1) `div` l
        pow d = product (replicate d b)

    -- Level d spans array indices [B^d - 1, B^(d+1) - 1).
    levelOf :: Int -> Int
    levelOf i = go 0 b
      where
        go !d !p
          | i < p - 1 = d
          | otherwise = go (d + 1) (p * b)

-- | Render a "Data.STree.BPlus" map: the index above, every key and value
-- below.
--
-- The point it is drawn to make is what separates a B+tree from a B-tree. Every
-- key is in the bottom row, in order and contiguous, with its value directly
-- beneath it; the tree above holds nothing but *copies* of some of those keys.
-- Those copies are outlined and shaded in the bottom row, so it is visible that
-- the index is derived from the leaves rather than holding data of its own.
--
-- Leaves alternate shade so their boundaries are legible, and every column
-- carries its position, which is what 'BP.keyAt' and 'BP.valueAt' take.
--
-- No edges run from the index to the leaves. The mapping is by leaf number
-- rather than by layout offset, so honest edges would need the index's in-order
-- ranks; the shared shading carries the correspondence instead, as it does for
-- the array row in 'toDotStyled'.
toDotBPlus ::
  forall li ll k v.
  (LineWidth li, LineWidth ll, Key k, U.Unbox v, Show k, Show v) =>
  BPlusTree li ll k v ->
  String
toDotBPlus t =
  unlines $
    [ "digraph SBPlusTree {"
    , "  graph [rankdir=TB, ordering=out, labelloc=\"t\"];"
    , "  label=" ++ quoted summary ++ ";"
    , "  node [shape=record, fontname=\"monospace\", fontsize=10, style=filled];"
    , "  edge [arrowsize=0.6];"
    ]
      ++ indexSection
      ++ [ "  leaves [shape=plaintext, fontname=\"monospace\", label=<" ++ leafTable ++ ">];"
         , "  legend [shape=plaintext, fontname=\"monospace\", label=<" ++ legend ++ ">];"
         , "  { rank=same; leaves; legend; }"
         ]
      ++ ["}"]
  where
    ll = lineWidth @ll
    n = BP.size t
    ks = BP.keys t
    vs = BP.values t
    ix = BP.indexTree t

    leafCount = max 1 ((n + ll - 1) `div` ll)
    padded = leafCount * ll

    summary =
      "static B+tree: "
        ++ show n
        ++ " keys, li="
        ++ show (lineWidth @li)
        ++ " ll="
        ++ show ll
        ++ ", "
        ++ show (BP.height t)
        ++ " levels -- descend the index to a leaf, then scan it"

    -- The index is an ordinary static B-tree over the separators, so it draws
    -- with the shared walk. Nodes are "s0, s2, ..." to keep them distinct.
    indexSection
      | size ix == 0 = ["  // the whole map fits in one leaf: no index"]
      | otherwise =
          renderNodes "s" show ix
            ++ ["  s" ++ show (leftmostDeepest ix) ++ " -> leaves [style=invis];"]

    -- Position p holds a separator exactly when it starts a leaf other than the
    -- first: separator j is the first key of leaf j+1.
    isSeparator p = p > 0 && p `rem` ll == 0 && p < n

    leafTable =
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"3\">"
        ++ row [headerCell j | j <- [0 .. leafCount - 1]]
        ++ row [keyCell p | p <- [0 .. padded - 1]]
        ++ row [valueCell p | p <- [0 .. padded - 1]]
        ++ row [posCell p | p <- [0 .. padded - 1]]
        ++ "</TABLE>"
      where
        row cells = "<TR>" ++ concat cells ++ "</TR>"

        headerCell j =
          "<TD COLSPAN=\"" ++ show ll ++ "\" BGCOLOR=\"" ++ leafShade j ++ "\">"
            ++ "<FONT POINT-SIZE=\"9\">leaf " ++ show j ++ "</FONT></TD>"

        keyCell p
          | p >= n = "<TD BGCOLOR=\"#e0e0e0\">~</TD>"
          | isSeparator p =
              "<TD BGCOLOR=\"" ++ levelColour 0 ++ "\"><B>"
                ++ escapeHtml (show (ks U.! p))
                ++ "</B></TD>"
          | otherwise =
              "<TD BGCOLOR=\"" ++ leafShade (p `div` ll) ++ "\">"
                ++ escapeHtml (show (ks U.! p))
                ++ "</TD>"

        valueCell p
          | p >= n = "<TD BGCOLOR=\"#e0e0e0\"></TD>"
          | otherwise =
              "<TD BGCOLOR=\"#ffffff\"><FONT POINT-SIZE=\"9\">"
                ++ escapeHtml (show (vs U.! p))
                ++ "</FONT></TD>"

        posCell p =
          "<TD BORDER=\"0\"><FONT POINT-SIZE=\"8\">" ++ show p ++ "</FONT></TD>"

    -- Two neutral shades, so leaf boundaries read without another hue.
    leafShade j = if even j then "#f2f2f2" else "#e2e2e2"

    legend =
      "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"3\">"
        ++ "<TR><TD COLSPAN=\"2\" BORDER=\"0\"><B>B+tree</B></TD></TR>"
        ++ entry (levelColour 0) "copied into the index"
        ++ entry "#f2f2f2" "ordinary key"
        ++ entry "#ffffff" "value"
        ++ entry "#e0e0e0" "padding"
        ++ "</TABLE>"
      where
        entry c d =
          "<TR><TD BGCOLOR=\"" ++ c ++ "\">  </TD>"
            ++ "<TD BORDER=\"0\"><FONT POINT-SIZE=\"9\">" ++ d ++ "</FONT></TD></TR>"

-- | The node records and the edges between them, shared by both renderers.
--
-- Walks with the same recurrence the descent uses: from a node with cursor @b@
-- at offset @(b-1)*L@, child @r@ is at cursor @off + b + r + 1@, and a child
-- exists exactly when its offset is still inside the data. Nodes are named
-- @<prefix><offset>@, so the caller can keep two trees apart in one graph.
renderNodes ::
  forall l a.
  (LineWidth l, U.Unbox a) =>
  String ->
  (a -> String) ->
  BTree l a ->
  [String]
renderNodes prefix render t = walk 0 1
  where
    l = lineWidth @l
    n = size t
    h = height t
    arr = layout t

    walk :: Int -> Int -> [String]
    walk !lev !cursor
      | off >= n = []
      | otherwise = nodeLine : concatMap child [0 .. l]
      where
        off = (cursor - 1) * l
        name = prefix ++ show off

        nodeLine =
          "  "
            ++ name
            ++ " [label="
            ++ quoted (recordLabel off)
            ++ ", fillcolor=\""
            ++ levelColour lev
            ++ "\"];"

        child r
          | lev >= h - 1 = []
          | off' >= n = []
          | otherwise =
              ("  " ++ name ++ ":p" ++ show r ++ " -> " ++ prefix ++ show off' ++ ";")
                : walk (lev + 1) cursor'
          where
            cursor' = off + cursor + r + 1
            off' = (cursor' - 1) * l

    -- "<p0>|k0|<p1>|k1|<p2>": a port before and after every key, so edges leave
    -- from the gap whose range they lead to.
    recordLabel off =
      intercalate "|" $
        concat [["<p" ++ show i ++ ">", cell (off + i)] | i <- [0 .. l - 1]]
          ++ ["<p" ++ show l ++ ">"]
      where
        cell i
          | i < n = escapeRecord (render (U.unsafeIndex arr i))
          | otherwise = "~" -- sentinel padding

-- | Offset of the leftmost node of the deepest level, reached by following
-- child 0 from the root. The leftmost subtrees are the ones the build fills
-- completely, so it always exists. Used to anchor a wide bottom row under the
-- tree rather than off to its right.
leftmostDeepest :: forall l a. LineWidth l => BTree l a -> Int
leftmostDeepest t = go 0 1
  where
    l = lineWidth @l
    go !lev !cursor
      | lev >= height t - 1 = (cursor - 1) * l
      | otherwise = go (lev + 1) ((cursor - 1) * l + cursor + 1)

-- | Light fills, so black text stays readable. Cycled if a tree is deeper than
-- the palette, which needs a fanout of 3 and a lot of keys.
levelColour :: Int -> String
levelColour d = palette !! (d `mod` length palette)
  where
    palette =
      [ "#cfe3f7"
      , "#d9f0d3"
      , "#fdf0c2"
      , "#f7d6cf"
      , "#e6d9f2"
      , "#d5f0ee"
      , "#f0e2d0"
      , "#ebebeb"
      ]

quoted :: String -> String
quoted s = '"' : concatMap esc s ++ "\""
  where
    esc c
      | c == '"' || c == '\\' = ['\\', c]
      | otherwise = [c]

-- | Inside a record label, the structural characters have to be escaped as
-- well, or a key containing one would silently split the record.
escapeRecord :: String -> String
escapeRecord = concatMap esc
  where
    esc c
      | c `elem` "|<>{}\" \\" = ['\\', c]
      | otherwise = [c]

-- | HTML-like labels are a different dialect: markup characters, not record
-- separators, are what need escaping.
escapeHtml :: String -> String
escapeHtml = concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc c = [c]
