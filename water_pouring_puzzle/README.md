```
From: Greg Davidson <greg.davidson@gmail.com>
Date: Sun, 16 Apr 2023 17:54:29 -0700
Message-ID: <CA+2rpgk4XWfs8u54kQR5V5SVFVdOBikRvu8f=v_75_jiAAaJAQ@mail.gmail.com>
Subject: Water Jugs Problem
To: Jeff Tamer <jeff@tamer.codes>
Content-Type: text/plain; charset="UTF-8"

This is the problem definition I'm using.
I'm going to start with a direct approach
and then maybe try generalizing it.  YMMV!

Love, _Touch

-- * Water Jugs Problem

-- ** Statement of Problem

-- *** Given:

-- a little jug which can hold 3 liters
-- a big jug which can hold 5 liters

-- *** Goal:

-- Achieve exactly 4 liters in the big jug
-- with the fewest number of actions,
-- starting with both jugs empty

-- *** Allowed actions:

-- Fill either jug to its full capacity
-- Empty either jug completely
-- Pour one jug into the other until either
-- - the jug being poured from is empty
-- - the jug being poured into is full

-- Optimal or Heuristic Search not required
-- - Brute-force Breadth-First would suffice,
-- - or other methods not more than a small
-- - constant worse in time and space resources.
```
