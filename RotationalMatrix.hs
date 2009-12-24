{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module RotationalMatrix where
import Vector3

data ThreeByThree v = TrpTrp v v v

class (Vector v) => Matrix r v where
	(..+) :: r -> r -> r
	(..*) :: DoubleType -> r -> r
	transpose :: r -> r
	--inverse :: r -> r
	vm :: v -> r -> v
	
instance Matrix (ThreeByThree Triple) Triple where
	(..+) (TrpTrp r0 u0 o0) (TrpTrp r1 u1 o1) = 
		TrpTrp (r0 .+ r1) (u0 .+ u1) (o0 .+ o1)
	(..*) scale (TrpTrp r0 u0 o0) = 
		TrpTrp (scale .* r0) (scale .* u0) (scale .* o0)
	transpose (TrpTrp (Trp r00 r01 r02) 
		(Trp r10 r11 r12) (Trp r20 r21 r22)) = TrpTrp (Trp r00 r10 r20) 
												(Trp r01 r11 r21) 
												(Trp r02 r12 r22)
	vm v (TrpTrp rt up out) = Trp (v `dot` rt) (v `dot` up) 
									(v `dot` out)
										
	--inverse (TrpTrp a b c)  = transpose (TrpTrp a b c)