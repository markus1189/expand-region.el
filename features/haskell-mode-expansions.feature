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
    And I press "C-2 C-@"
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
