module Suite exposing (suite)

import Expect
import Rope
import Test exposing (Test, test)


suite : Test
suite =
    test "fromList/toList" <|
        \_ ->
            [ 1, 2, 3 ]
                |> Rope.fromList
                |> Rope.toList
                |> Expect.equalLists [ 1, 2, 3 ]
