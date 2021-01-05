{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
{-# OPTIONS_GHC -Wall     #-}

module Lib (plugin) where


import           Class hiding (className)
import           Control.Monad
import           Data.Bifunctor (second)
import           Data.Generics (everything, mkQ)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable (for)
import           Data.Tuple (swap)
import           GHC.TcPluginM.Extra (lookupModule, lookupName)
import           GhcPlugins
import           InstEnv
import           Prelude hiding ((<>), pred)
import           TcEvidence
import           TcPluginM
import           TcRnTypes
import           TcType

plugin :: Plugin
plugin = magicTyFamPlugin "debug" "Debug" "Debug" keep $ \ty -> do
  inst_env <- getInstEnvs
  show_cls <- tcLookupClass =<< lookupModuleAndName "base" "GHC.Show" "Show"
  succeed_cls <- tcLookupClass =<< lookupModuleAndName "debug" "Debug" "Succeed"
  let Just succ_inst = findInst inst_env succeed_cls [ty]
  let (insts, _, _) = lookupInstEnv False inst_env show_cls [ty]
  case insts of
    (_, _) : _ -> do
      Just dict <- buildDict (error "nah") $ mkTyConApp (classTyCon show_cls) [ty]
      pure $ Just $
        -- pprPanic "made a" $ ppr $
          evDFunApp (is_dfun succ_inst) [ty] [getEvExpr dict]
    _ -> do
      dict <- unsafeTcPluginTcM $ mkUnknownDebugTypeDict ty
      pure $ Just dict




keep :: [Type] -> Maybe Type
keep [ty] = maybe (Just ty) Just $ tcView ty
keep _ = Nothing


lookupModuleAndName :: String -> String -> String -> TcPluginM Name
lookupModuleAndName p m i = do
  md <- lookupModule (mkModuleName m) $ fsLit p
  lookupName md $ mkTcOcc i



------------------------------------------------------------------------------
-- | Get your hands on the type constructor for your type family.
loadTyCon
    :: String  -- ^ package
    -> String  -- ^ module
    -> String  -- ^ identifier
    -> TcPluginM TyCon
loadTyCon p m i = do
  nm <- lookupModuleAndName p m i
  fmap classTyCon $ tcLookupClass nm


------------------------------------------------------------------------------
-- | Generate a plugin that solves instances of the type family pointed at by
-- the three strings, via the function.
magicTyFamPlugin
    :: String  -- ^ package
    -> String  -- ^ module
    -> String  -- ^ identifier
    -> ([Type] -> Maybe a)
    -> (a -> TcPluginM (Maybe EvTerm))
    -> Plugin
magicTyFamPlugin p m i f tc =
  defaultPlugin
  { tcPlugin = const $ Just $ TcPlugin
      { tcPluginInit = loadTyCon p m i
      , tcPluginSolve = \cmp_type _ _ w ->
          TcPluginOk <$> solveWanteds f tc cmp_type w <*> pure []
      , tcPluginStop = const $ pure ()
      }
  , pluginRecompile = const $ pure NoForceRecompile
  }



------------------------------------------------------------------------------
-- | @solveWanteds f tyfam cts@ finds all instaces of @tyfam@ inside the wanted
-- constraints @cts@, and evaluates them via @f@. The result is a set of
-- 'CNonCanonical' constraints, which should be emitted as the second parameter
-- of 'TcPluginOk'.
solveWanteds :: ([Type] -> Maybe a) -> (a -> TcPluginM (Maybe EvTerm)) -> TyCon -> [Ct] -> TcPluginM [(EvTerm, Ct)]
solveWanteds _ _ _ [] = pure []
solveWanteds f tc cmp_type wanted = do
  let rel = fmap (findRelevant f cmp_type <*> ctev_pred . cc_ev) wanted

  for (concat rel) $ \(MagicTyFamResult ct _ res) -> do
    Just ev <- tc res
    pure (ev, ct)


------------------------------------------------------------------------------
-- | Locate and expand the use of any type families.
findRelevant :: ([Type] -> Maybe a) -> TyCon -> Ct -> Type -> [MagicTyFamResult a]
findRelevant f cmp_type ct = everything (++) $ mkQ [] findCmpType
  where
    findCmpType t =
      case splitTyConApp_maybe t of
        Just (tc, ts) | tc == cmp_type ->
           maybe [] (pure . MagicTyFamResult ct t) $ f ts
        _ -> []


data MagicTyFamResult a = MagicTyFamResult
  { _mtfrLoc      :: Ct
  , _mtfrOriginal :: Type
  , _mtfrSolved   :: a
  }








------------------------------------------------------------------------------
-- | Match an instance head against a concrete type; returning a substitution
-- from one to the other. If this returns 'mempty', there were no type
-- variables to match.
match
    :: PredType  -- class inst
    -> PredType  -- concrete type
    -> Map TyVar Type
match instClass concClass =
  let Just (_, instHead) = splitAppTy_maybe instClass
      Just (_, concHead) = splitAppTy_maybe concClass
      (_, instTys) = splitAppTys instHead
      (_, concTys) = splitAppTys concHead
   in M.fromList . mapMaybe (fmap swap . sequence . second getTyVar_maybe)
                 $ zip concTys instTys



------------------------------------------------------------------------------
-- | Substitute all type variables in a 'Type'.
-- TODO(sandy): this should probably operate over a 'PredType' and explicily
-- split and fold, rather than assuming a shape of 'C a b c'.
instantiateHead
    :: Map TyVar Type
    -> Type
    -> Type
instantiateHead mmap t =
  let (tc, tys) = splitAppTys t
      tys' = fmap (\ty -> maybe ty (mmap M.!) $ getTyVar_maybe ty) tys
   in mkAppTys tc tys'


------------------------------------------------------------------------------
-- | Construct an 'EvTerm' witnessing an instance of 'wantedDict' by
-- recursively finding instances, and building dicts for their contexts.
buildDict
    :: CtLoc
    -> PredType
    -> TcPluginM (Maybe EvTerm)
buildDict loc wantedDict = do
  (className, classParams) <-
    case splitTyConApp_maybe wantedDict of
      Just a  -> pure a
      Nothing -> pprPanic "buildDict given something that isnt a pred type to solve"
               $ ppr wantedDict

  myclass <- tcLookupClass $ tyConName className
  envs    <- getInstEnvs

  case lookupUniqueInstEnv envs myclass classParams of
    -- success!
    Right (clsInst, _) -> do
      let dfun = is_dfun clsInst
          (vars, subclasses, inst) = tcSplitSigmaTy $ idType dfun
          mmap = match inst wantedDict

      if null subclasses
         then pure . Just $ evDFunApp dfun [] []
         else do
           mayDicts <- traverse (buildDict loc . instantiateHead mmap) subclasses

           for (sequence mayDicts) $ \dicts ->
             pure $ evDFunApp dfun (fmap (mmap M.!) vars) $ fmap getEvExpr dicts

    Left _ -> do
      -- check givens?
      pure Nothing


mkUnknownDebugTypeDict :: Type -> TcM EvTerm
mkUnknownDebugTypeDict ty = do
  str <- mkStringExpr $ mconcat ["<debug: (", unsafeRender ty, ")>"]
  pure $ EvExpr $ mkCoreLams [mkWildValBinder ty] str


getEvExpr :: EvTerm -> EvExpr
getEvExpr (EvExpr e) = e
getEvExpr e = pprPanic "getEvExpr: somehow generated a non EvExpr term" $ ppr e

findInst :: InstEnvs -> Class -> [Type] -> Maybe ClsInst
findInst envs cls ts =
  case lookupUniqueInstEnv envs cls ts of
    Right (inst, _) ->
      pure inst
    Left _ -> Nothing


unsafeRender :: Outputable a => a -> String
unsafeRender = showSDoc unsafeGlobalDynFlags . ppr


