
\chapter[SubType] {Extensible Union Types}

\begin{comment}
\begin{code}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
module InterpreterLib.SubType (SubType, inj, prj, returnInj,checkType,liftM2Sub, liftMSub) where
import Control.Monad.Error
\end{code}
\end{comment}

\begin{code}
class SubType a b where
 inj :: a -> b       
 prj :: b -> Maybe a 

instance SubType a (Either a x) where
 inj       = Left
 prj       = either Just (const Nothing)

-- SubType Double (Either Double ()) => Subtype Double (Either Bool (Either Double ()))
instance (SubType a b) => SubType a (Either x b) where
   inj       = Right . inj
   prj       = either (const Nothing) prj


instance SubType x x
 where
  inj = id
  prj = Just


returnInj :: (SubType a b , Monad m) => a -> m b
returnInj = return . inj

checkType x =  maybe (throwError $ strMsg ("Type error: Cannot project to the desired type"))
                     return 
                     (prj x)



liftMSub f a = do ap <- a
                  av <- checkType ap
                  returnInj $ f av

liftM2Sub op a b = do ap <- a
                      bp <- b
                      av <- checkType ap
                      bv <- checkType bp
                      returnInj $ av `op` bv

\end{code}
