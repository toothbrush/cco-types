import CCO.Component    (Component, component, printer, ioWrap)
import CCO.SystemF
import CCO.HM
import CCO.Tree         (ATerm, toTree, parser, fromTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap $
        CCO.Tree.parser >>> 
        (component toTree :: Component ATerm CCO.HM.Tm) >>>
        gimme >>> 
        (arr fromTree) >>>
        printer

-- | constructs a really simple term, for testing.
gimme :: Component CCO.HM.Tm CCO.SystemF.Tm
gimme = component $ (\s-> do return (CCO.SystemF.Var "s"))
