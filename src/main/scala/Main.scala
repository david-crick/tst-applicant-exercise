object Main extends App {

    println("PROBLEM 1")
    println("=========")

    println("BEST GROUP PRICES")

    val bestGroupPrices = PriceTool.getBestGroupPrices(
        Seq(
            Rate("M1", "Military"),
            Rate("M2", "Military"),
            Rate("S1", "Senior"),
            Rate("S2", "Senior")
        ),
        Seq(
            CabinPrice("CA", "M1", 200.00),
            CabinPrice("CA", "M2", 250.00),
            CabinPrice("CA", "S1", 225.00),
            CabinPrice("CA", "S2", 260.00),
            CabinPrice("CB", "M1", 230.00),
            CabinPrice("CB", "M2", 260.00),
            CabinPrice("CB", "S1", 245.00),
            CabinPrice("CB", "S2", 270.00)
        )
    )

    println(bestGroupPrices)
    println()

    println("PROBLEM 2")
    println("=========")

    val allPromotions = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4", "P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2"))
    )

    println("ALL PROMOTION COMBINATIONS")
    println(PriceTool.allCombinablePromotions(allPromotions))
    println()

    println("COMBINABLE PROMOTIONS FOR P1")
    println(PriceTool.combinablePromotions("P1", allPromotions))
    println()
    
    println("COMBINABLE PROMOTIONS FOR P3")
    println(PriceTool.combinablePromotions("P3", allPromotions))
    println()
}
