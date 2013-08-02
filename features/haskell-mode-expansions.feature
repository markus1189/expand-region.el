Feature: haskell-mode expansions
  In order to quickly and precisely mark haskell code
  As an Emacs user
  With haskell-mode installed
  I want to expand to them

  Scenario: Mark `where' block
    Given I turn on haskell-mode
    When I insert:
    """
    value :: (String,String)
    value = (question,answer)
    where
      question = undefined
      answer = show $ product [1 2 21]
    """
    And I place the cursor before "question"
    And I press "C-3 C-@"
    Then the region should be:
    """

      question = undefined
      answer = show $ product [1 2 21]
    """
    And I press "C-@"
    Then the region should be:
    """
    where
      question = undefined
      answer = show $ product [1 2 21]
    """


  Scenario: Mark one line `where' block
    Given I turn on haskell-mode
    When I insert:
    """
    value = answer
      where answer = 42
    """
    And I place the cursor before "answer"
    And I press "C-2 C-@"
    Then the region should be:
    """
     answer = 42
    """
    And I press "C-@"
    Then the region should be:
    """
    where answer = 42
    """

  Scenario: Mark `where' block that has empty lines
    Given I turn on haskell-mode
    When I insert:
    """
    foo = undefined
      where
        a = 1
        b = 2

        c = 3
    """
    And I place the cursor before "b"
    And I press "C-2 C-@"
    Then the region should be:
    """

        a = 1
        b = 2

        c = 3
    """
    And I press "C-@"
    Then the region should be:
    """
    where
        a = 1
        b = 2

        c = 3
    """

  Scenario: Mark one line declaration body
    Given I turn on haskell-mode
    When I insert:
    """
    foo = putStrLn "So long, and thanks for all the fish."
    """
    And I place the cursor before "putStrLn"
    And I press "C-2 C-@"
    Then the region should be:
    """
    putStrLn "So long, and thanks for all the fish."
    """

  Scenario: Mark multiline declaration body
    Given I turn on haskell-mode
    When I insert:
    """
    foo :: Int
    foo = product [ 1
                  , 2
                  , 21
                  ]
    """
    And I place the cursor before "product"
    And I press "C-2 C-@"
    Then the region should be:
    """
    product [ 1
                  , 2
                  , 21
                  ]
    """

  Scenario: Mark one line declaration completely
    Given I turn on haskell-mode
    When I insert:
    """

    foo = putStrLn "So long, and thanks for all the fish."

    """
    And I place the cursor before "putStrLn"
    And I press "C-u 3 C-@"
    Then the region should be:
    """
    foo = putStrLn "So long, and thanks for all the fish."
    """

  Scenario: Mark declaration with type signature
    Given I turn on haskell-mode
    When I insert:
    """

    foo :: Int
    foo = product [ 1
                  , 2
                  , 21
                  ]

    """
    And I place the cursor before "product"
    And I press "C-4 C-@"
    Then the region should be:
    """
    foo :: Int
    foo = product [ 1
                  , 2
                  , 21
                  ]
    """

  Scenario: Mark declaration up to where clause
    Given I turn on haskell-mode
    When I insert:
    """
    foo = 42 * value
      where value = 1
    """
    And I place the cursor before "42"
    And I press "C-2 C-@"
    Then the region should be:
    """
    42 * value
    """

  Scenario: Mark body of do syntax
    Given I turn on haskell-mode
    When I insert:
    """
    deepThink = do
      putStrLn "The answer is..."
      putStrLn answer
      where
        answer = "42"
    """
    And I place the cursor before "putStrLn"
    And I press "C-2 C-@"
    Then the region should be:
    """

      putStrLn "The answer is..."
      putStrLn answer
    """

  Scenario: Does not mark multiple declarations
    Given I turn on haskell-mode
    When I insert:
    """
    main :: IO ()
    main = do
      size <- maybe 8 read . listToMaybe <$> getArgs
      printLines (queens size :: [Board])
      printLines . observeAll $ (queens size :: Logic Board)
      where
        printLines :: Show a => [a] -> IO ()
        printLines = putStrLn . unlines . fmap show

    queens :: MonadPlus m => Int -> m Board
    queens n = placeQueens n n

    placeQueens :: MonadPlus m => Int -> Int -> m Board
    placeQueens _ 0 = return mzero
    placeQueens size k = do
      queens' <- placeQueens size (k-1)
      col <- msum [return x | x <-[1..size]]
      let queen = (k,col)
      guard $ queen `safeOn` queens'
      return (queen:queens')

    safeOn :: Queen -> Board -> Bool
    safeOn q = not . any (inCheck q)

    inCheck :: Queen -> Queen -> Bool
    inCheck p1@(x1,y1) p2@(x2,y2) = x1 == x2 || y1 == y2 || p1 `isDiagonalTo` p2

    isDiagonalTo :: Queen -> Queen -> Bool
    isDiagonalTo (x1,y1) (x2,y2) = abs (x1 - x2) == abs (y1 - y2)
    """
    And I place the cursor before "printLines"
    And I press "C-3 C-@"
    # complete where block
    Then the region should be:
    """
    where
        printLines :: Show a => [a] -> IO ()
        printLines = putStrLn . unlines . fmap show
    """
    # do body with where clause
    And I press "C-@"
    Then the region should be:
    """

      size <- maybe 8 read . listToMaybe <$> getArgs
      printLines (queens size :: [Board])
      printLines . observeAll $ (queens size :: Logic Board)
      where
        printLines :: Show a => [a] -> IO ()
        printLines = putStrLn . unlines . fmap show
    """
    # mark do with body; mark complete declaration
    And I press "C-2 C-@"
    Then the region should be:
    """
    main = do
      size <- maybe 8 read . listToMaybe <$> getArgs
      printLines (queens size :: [Board])
      printLines . observeAll $ (queens size :: Logic Board)
      where
        printLines :: Show a => [a] -> IO ()
        printLines = putStrLn . unlines . fmap show
    """
    # the last step marks the type signature as well
    And I press "C-@"
    Then the region should be:
    """
    main :: IO ()
    main = do
      size <- maybe 8 read . listToMaybe <$> getArgs
      printLines (queens size :: [Board])
      printLines . observeAll $ (queens size :: Logic Board)
      where
        printLines :: Show a => [a] -> IO ()
        printLines = putStrLn . unlines . fmap show
    """
