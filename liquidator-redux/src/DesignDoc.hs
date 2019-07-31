module DesignDoc
  (
    -- $architecture
    -- $identdesign
    -- $schemadesign
    -- $authdesign
  ) where

-- |
-- $architecture
-- = General component architecture
--
-- All components follow the @Handle@ pattern.  A single 'Handle' carries all
-- state, and all operations pertaining to the outermost layer of the
-- application occur in 'IO'.
--
-- All business logic is implemented as pure functions.  An adapter layer
-- mediates data between the outer and the innermost layer, as necessary.
--
-- Conceptually, a component is
--
-- >> data Input   -- All inputs gathered from external sources
-- >> data Output  -- All outputs for external consumers
-- >> businessLogic :: Input -> Output
-- >> getInput :: IO Input
-- >> putOutput :: Output -> IO ()
-- >> component :: IO ()
-- >> component = getInput >>= pure . component >>= putOutput

------------------------------------------------------------------------------

-- |
-- $identdesign
-- = Internal vs external identifiers
--
-- Generic, internal identifiers are 64-bit numbers.  Ideally, these
-- identifiers should not be user visible.
--
-- For externally visible surrogate keys, we want monotonically increasing yet
-- hard-to-predict values.

------------------------------------------------------------------------------

-- |
-- $schemadesign
-- = JSON schema mappings
--
-- For each schema, we typically define two structures: one for the body
-- comprising the actual data and one that wraps the body.
--
-- As a form of namespacing, records are prefixed by all-lowercase abbreviated
-- version of the record type name.  When converting to/from JSON, the prefix
-- is stripped and everything after the prefix is converted to @snake_case@.

------------------------------------------------------------------------------

-- |
-- $authdesign
-- = Authorization
--
-- Two types of tokens
--
-- [@refresh@] Long-lived token, granted based on credentials; a refresh
-- token, in turn, may be used to acquire @access@ tokens.
--
-- [@access@]  A token that grants access to a protected resource.
--
-- The idea is that we only need to authenticate the user when the @refresh@
-- token is handed out and then use @access@ tokens to regulate access to
-- protected resources, not having to re-authenticate until the @refresh@
-- token is invalidated.
