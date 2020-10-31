

-type mail() :: any().
-type data() :: any().
-type label() :: any().
-type result() :: {done, data()} | inprogress .
-type labelled_result() :: {label(), result()}.
-type filter_result() :: {just, data()}
    | {transformed, mail()}
    | unchanged
    | {both, mail(), data()}.
-type filter_fun() :: fun( (mail(), data()) -> filter_result() ).