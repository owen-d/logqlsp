// Model Errors !format

// #specification     unexpected:          <T> <got> <next type>
// #test              {|=}                 (FilterExpr, LabelMatcher)
// #test              sum("hello")         (String, MetricExpr<Stubby>)
//
// #ignore            Implement a Stubby (stubbable) struct to be
//                    used as a dummy type/non-overriding parameter.
// #hint              Use  PhantomData as a trait to map to the valid
//                    variants allowed in this instance of MetricExpr,
//                    in this case MetricExpr<SumTypePicker>.
//
// #test              sum([5m])            (LogRange, MetricExpr<Stubby>)
// #specification     unstarted input:     <T> ", `, (, [, {
// #test              rate|                (Delimiter, Delimiter)
// #test
// #test              sum)                 (Delimiter, Delimiter)
// #test              sumQ                 (Token, Delimiter)
// #specification     unterminated input:  <T> ", `, ), ], }
// #test              sum("hello"          (Delimiter, Nothing)
//

// Generate a list of tests below with matching signatures:
// 1 + 1 => int
// 1 - 3 => int
// 2 * 3 => int
// 6 / 4 => float
