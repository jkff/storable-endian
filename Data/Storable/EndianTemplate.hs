module Data.Storable.EndianTemplate where

import Data.Char

import Unsafe.Coerce
import Foreign.Ptr

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad

concatM :: (Monad m) => [m [a]] -> m [a]
concatM [] = return []
concatM (m:ms) = liftM2 (++) m (concatM ms)

deriveEndian :: String -> Int -> Int -> Q [Dec]
deriveEndian baseName bytes bits = concatM [mapM declareNewtype ["le", "be"], mapM deriveEndian' ["le", "be"]]
  where
    declareNewtype end = do
      baseType <- conT (mkName baseName)
      let typeName = baseName ++ map toUpper end
      newtypeD (cxt []) (mkName typeName) [] (return $ NormalC (mkName typeName) [(NotStrict, baseType)]) []
    deriveEndian' end = do
      let typeName = mkName $ baseName ++ map toUpper end
      let getter   = mkName $ "getWord" ++ show bits ++ end
      let putter   = mkName $ "putWord" ++ show bits ++ end
      let vp = mkName "p"
      let vx = mkName "x"
      pokeBody <- [| $(varE putter) (unsafeCoerce $(varE vx)) (castPtr $(varE vp))|]
      members <- concatM [
          [d| sizeOf _ = $(lift bytes) |]
         ,[d| alignment _ = $(lift bytes) |]
         ,[d| peek p = ($(conE typeName) . unsafeCoerce) `fmap` $(varE getter) (castPtr p) |]
         ,return [FunD (mkName "poke") [Clause 
            [VarP (mkName "p"), ConP typeName [VarP (mkName "x")]] 
            (NormalB pokeBody)
            []]]
        ]
      instanceD (cxt []) (appT (conT (mkName "Storable")) (conT typeName)) (map return members)
