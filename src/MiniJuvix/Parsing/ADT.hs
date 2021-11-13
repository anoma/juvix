-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- |  Adapted from heliaxdev/Juvix/library/StandardLibrary/src/Juvix/
module MiniJuvix.Parsing.ADT where

--------------------------------------------------------------------------------

-- import qualified Data.Aeson as A
import MiniJuvix.Utils.Prelude

--------------------------------------------------------------------------------

-- type ConstructorName = NameSymb

-- type NameSymb = NonEmpty Symbol

-- type ModuleName = NameSymb

-- data TopLevel
--   = Type Type
--   | ModuleOpen ModuleOpen
--   | Signature Signature
--   | Module Module
--   | Function Function
--   | Effect Effect
--   | Handler Handler
--   | Declaration Declaration
--   | TypeClass
--   | TypeClassInstance
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Declarations
-- --------------------------------------------------------------------------------

-- newtype Declaration
--   = Infixivity InfixDeclar
--   deriving stock (Show, Read, Eq)

-- data InfixDeclar
--   = NonAssoc Symbol Natural
--   | AssocL Symbol Natural
--   | AssocR Symbol Natural
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Types
-- --------------------------------------------------------------------------------

-- data Type
--   = Typ
--       -- Was a usage but can't alias for now
--       { typeUsage :: Maybe Expression,
--         typeName' :: !Symbol,
--         typeArgs :: [Symbol],
--         typeForm :: Data
--         -- TODO: To support Empty type this should be 'Maybe Data'
--       }
--   deriving stock (Show, Read, Eq)

-- -- | 'Data' is the data declaration in the Juvix language
-- data Data
--   = Arrowed
--       { dataArrow :: Expression,
--         dataAdt' :: Adt
--       }
--   | NonArrowed
--       { dataAdt :: Adt
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Arrows
-- --------------------------------------------------------------------------------

-- data NamedType
--   = NamedType'
--       { nameRefineName :: !Name,
--         namedRefineRefine :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- -- TODO ∷ change TypeName to TypeNameModule
-- data TypeRefine
--   = TypeRefine
--       { typeRefineName :: Expression,
--         typeRefineRefinement :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Types Misc
-- --------------------------------------------------------------------------------

-- data Name
--   = Implicit !Symbol
--   | Concrete !Symbol
--   deriving stock (Show, Read, Eq)

-- data ArrowSymbol
--   = ArrowUse Usage.T
--   | -- Was a usage but can't alias for now
--     ArrowExp Expression
--   deriving stock (Show, Read, Eq)

-- -- TODO ∷ finish this type!
-- newtype UniverseExpression
--   = UniverseExpression Symbol
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- ADTs
-- --------------------------------------------------------------------------------

-- -- The types for ADT are not the most constraining, however are setup
-- -- to have the least amount of boilerplate in the latter stages as
-- -- possible a Sum of length one should not have a record
-- -- [type Address = Foo {abc : Int}]
-- -- this form should be considered illegal unless we wish to permit
-- -- named records along with unnamed records. Ι suspect in the future
-- -- this will instead be used for Enum Subsets with refined information

-- data Adt
--   = Sum (NonEmpty Sum)
--   | Product Product
--   deriving stock (Show, Read, Eq)

-- data Sum
--   = S
--       { sumConstructor :: !Symbol,
--         sumValue :: !(Maybe Product)
--       }
--   deriving stock (Show, Read, Eq)

-- -- For when a product is without a sum only a record can apply a sum
-- -- of only one is a named product
-- data Product
--   = Record !Record
--   | Arrow Expression
--   | ADTLike [Expression]
--   deriving stock (Show, Read, Eq)

-- data Record
--   = Record''
--       { recordFields :: NonEmpty NameType,
--         recordFamilySignature :: Maybe Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data NameType
--   = NameType'
--       { nameTypeSignature :: Expression,
--         nameTypeName :: !Name,
--         nameTypeUsage :: Maybe Expression
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Effect Handlers
-- --------------------------------------------------------------------------------

-- data Effect
--   = Eff
--       { effName :: Symbol,
--         effOps :: [Signature]
--       }
--   deriving stock (Show, Read, Eq)

-- data Operation = Op (FunctionLike Expression)
--   deriving stock (Show, Read, Eq)

-- --  A 'Handler', as implemented here, is a set of functions that implement
-- -- (at least) one `Effect` interface.
-- -- However, Handlers are NOT scoped, meaning that they can't be defined
-- -- defined within another function. We CAN fix that, but it requires
-- -- us to make some choices, it's wise to have Witch up and running first.
-- data Handler
--   = Hand Symbol [Operation]
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Functions and Modules
-- --------------------------------------------------------------------------------

-- -- | 'Function' is a normal signature with a name arguments and a body
-- -- that may or may not have a guard before it
-- newtype Function
--   = Func (FunctionLike Expression)
--   deriving stock (Show, Read, Eq)

-- -- 'Module' is like function, however it allows multiple top levels
-- newtype Module
--   = Mod (FunctionLike (NonEmpty TopLevel))
--   deriving stock (Show, Read, Eq)

-- data ModuleE
--   = ModE
--       { moduleEBindings :: FunctionLike (NonEmpty TopLevel),
--         moduleEBody :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- -- 'FunctionLike' is the generic version for both modules and functions
-- data FunctionLike a
--   = Like
--       { functionLikedName :: Symbol,
--         functionLikeArgs :: [Arg],
--         functionLikeBody :: GuardBody a
--       }
--   deriving stock (Show, Read, Eq)

-- -- 'GuardBody' determines if a form is a guard or a body
-- data GuardBody a
--   = Body a
--   | Guard (Cond a)
--   deriving stock (Show, Read, Eq)

-- newtype ModuleOpen
--   = Open ModuleName
--   deriving stock (Show, Read, Eq)

-- data ModuleOpenExpr
--   = OpenExpress
--       { moduleOpenExprModuleN :: ModuleName,
--         moduleOpenExprExpr :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- -- Very similar to name, but match instead of symbol
-- data Arg
--   = ImplicitA MatchLogic
--   | ConcreteA MatchLogic
--   deriving stock (Show, Read, Eq)

-- newtype Cond a
--   = C (NonEmpty (CondLogic a))
--   deriving stock (Show, Read, Eq)

-- data CondLogic a
--   = CondExpression
--       { condLogicPred :: Expression,
--         condLogicBody :: a
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Signatures
-- --------------------------------------------------------------------------------

-- data Signature
--   = Sig
--       { signatureName :: Symbol,
--         -- Was a usage but can't alias for now
--         signatureUsage :: Maybe Expression,
--         signatureArrowType :: Expression,
--         signatureConstraints :: [Expression]
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Expression
-- --------------------------------------------------------------------------------

-- -- TODO ∷ add <expression> : <expression> <refine>?
-- -- to the parser
-- data Expression
--   = Cond (Cond Expression)
--   | Constant Constant
--   | Let Let
--   | ModuleE ModuleE
--   | LetType LetType
--   | Match Match
--   | Name NameSymb
--   | OpenExpr ModuleOpenExpr
--   | Lambda Lambda
--   | Application Application
--   | Primitive Primitive
--   | List List
--   | Tuple Tuple
--   | Block Block
--   | Infix Infix
--   | ExpRecord ExpRecord
--   | RecordDec Record
--   | Do Do
--   | EffApp EffApp
--   | -- Added due to merge
--     ArrowE ArrowExp
--   | NamedTypeE NamedType
--   | RefinedE TypeRefine
--   | UniverseName UniverseExpression
--   | Parened Expression
--   | DeclarationE DeclarationExpression
--   deriving stock (Show, Read, Eq)

-- data DeclarationExpression
--   = DeclareExpression Declaration Expression
--   deriving stock (Show, Read, Eq)

-- data Primitive
--   = Prim NameSymb
--   deriving stock (Show, Read, Eq)

-- data List
--   = ListLit [Expression]
--   deriving stock (Show, Read, Eq)

-- data Tuple
--   = TupleLit [Expression]
--   deriving stock (Show, Read, Eq)

-- data ArrowExp
--   = Arr'
--       { arrowExpLeft :: Expression,
--         -- Was a usage but can't alias for now
--         arrowExpUsage :: Expression,
--         arrowExpRight :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data Constant
--   = Number Numb
--   | String String'
--   deriving stock (Show, Read, Eq)

-- data Numb
--   = Integer' Integer
--   | Double' Double
--   deriving stock (Show, Read, Eq)

-- newtype String'
--   = Sho Text
--   deriving stock (Show, Read, Eq)

-- newtype Block
--   = Bloc
--       {blockExpr :: Expression}
--   deriving stock (Show, Read, Eq)

-- data Lambda
--   = Lamb
--       { lambdaArgs :: NonEmpty MatchLogic,
--         lambdaBody :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data Application
--   = App
--       { applicationName :: Expression,
--         applicationArgs :: NonEmpty Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data EffApp
--   = Via
--       { effappHand :: Expression,
--         effappArg :: Expression
--       }
--   deriving (Show, Read, Generic, Eq)

-- -- Was a newtype but extensible adds fields
-- newtype Do
--   = Do'' (NonEmpty DoBody)
--   deriving stock (Show, Read, Eq)

-- -- promote this to a match!!!
-- data DoBody
--   = DoBody
--       { doBodyName :: Maybe Symbol,
--         doBodyExpr :: Computation -- computation as in effect
--       }
--   deriving stock (Show, Read, Eq)

-- data Computation
--   = DoOp DoOp
--   | DoPure DoPure
--   deriving stock (Show, Read, Eq)

-- data DoOp
--   = DoOp'
--       { opName :: Expression,
--         opArgs :: NonEmpty Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data DoPure
--   = DoPure'
--       { pureArg :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- -- TODO ∷ we need includes in here as well!
-- -- Was a newtype but extensible adds fields
-- newtype ExpRecord
--   = ExpressionRecord
--       { expRecordFields :: NonEmpty (NameSet Expression)
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Symbol Binding
-- --------------------------------------------------------------------------------

-- data Let
--   = Let'
--       { letBindings :: FunctionLike Expression,
--         letBody :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data LetType
--   = LetType''
--       { letTypeBindings :: Type,
--         letTypeBody :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- data Infix
--   = Inf
--       { infixLeft :: Expression,
--         infixOp :: NameSymb,
--         infixRight :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- --------------------------------------------------------------------------------
-- -- Matching
-- --------------------------------------------------------------------------------

-- data Match
--   = Match''
--       { matchOn :: Expression,
--         matchBindigns :: NonEmpty MatchL
--       }
--   deriving stock (Show, Read, Eq)

-- data MatchL
--   = MatchL
--       { matchLPattern :: MatchLogic,
--         matchLBody :: Expression
--       }
--   deriving stock (Show, Read, Eq)

-- -- TODO ∷ add literals to the match
-- data MatchLogic
--   = MatchLogic
--       { matchLogicContents :: MatchLogicStart,
--         matchLogicNamed :: Maybe Symbol
--       }
--   deriving stock (Show, Read, Eq)

-- data MatchLogicStart
--   = MatchCon ConstructorName [MatchLogic]
--   | MatchName Symbol
--   | MatchConst Constant
--   | MatchRecord (NonEmpty (NameSet MatchLogic))
--   deriving stock (Show, Read, Eq)

-- data NameSet t
--   = Punned NameSymb
--   | NonPunned NameSymb t
--   deriving stock (Show, Read, Eq)

-- data Header topLevel
--   = Header NameSymb [topLevel]
--   | NoHeader [topLevel]
--   deriving stock (Show, Read, Eq)
