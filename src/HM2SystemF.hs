import CCO.Component (Component, component, printer, ioWrap)
import CCO.HM.Base (Tm)
import CCO.HM2SystemF (doConversion)
import qualified CCO.SystemF.Base as SF (Tm (Var))
import CCO.Tree (ATerm, toTree, parser, fromTree)
import Control.Arrow (arr, (>>>))

main = ioWrap $
        parser >>>
        (component toTree :: Component ATerm Tm) >>>
        convertAndType >>>
        (arr fromTree :: Component SF.Tm ATerm) >>>
        printer

-- | constructs a really simple term, for testing.
convertAndType :: Component Tm SF.Tm
convertAndType = component $ (\s -> do return (doConversion s))

