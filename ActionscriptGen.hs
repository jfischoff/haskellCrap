--Get geometry types working
--Get the actionscript equivalent
--

data Test a = Test {thing :: a}
data OtherThing a = OtherThing {huh :: Test a}

and = OtherThing (Test "ds")
