import collection.mutable.Map

/** Tools to help with pricing and promotions */
object PriceTool {

    /** For each cabin, gets best price for each rate group
     *
     *  @param rates the rate codes defined per rate group
     *  @param prices cabin prices by rate codes
     *  @return the best group prices for each cabin
     */
    def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]) : Seq[BestGroupPrice] = {

        //
        //  create a map for looking up rate group by rate code
        //

        val rateGroupMap = Map[String,String]()
        for (rate <- rates) {
            rateGroupMap += rate.rateCode -> rate.rateGroup
        }

        //
        //  create a map from cabin code to a map from rate code to best cabin price
        //

        val cabinGroupBestPriceMap = Map[String,Map[String,CabinPrice]]()
        for (price <- prices) {
            val rateGroup = rateGroupMap(price.rateCode)
            cabinGroupBestPriceMap get price.cabinCode match {
                case None => {
                    cabinGroupBestPriceMap += price.cabinCode -> Map[String,CabinPrice](rateGroup -> price)
                }
                case Some(groupBestPriceMap) => {
                    val currentBestPrice = groupBestPriceMap get rateGroup
                    if (currentBestPrice.isEmpty || currentBestPrice.get.price > price.price) {
                        groupBestPriceMap += rateGroup -> price
                    }
                }
            }
        }

        //
        //  return sequence of best group prices
        //

        var seq : Seq[BestGroupPrice] = Seq[BestGroupPrice]()
        cabinGroupBestPriceMap.foreach{ case (cabinCode, groupBestPriceMap) => groupBestPriceMap.foreach{ case (rateGroup, bestPrice) => seq = seq :+ BestGroupPrice(cabinCode, bestPrice.rateCode, bestPrice.price, rateGroup) }}
        return seq
    }

    /** builds map from promotion code to set of promotion codes that are not combinable with the promotion
     *
     *  @param allPromotions sequence of all promotions
     *  @return promotion restriction map
     */
    private def buildPromotionRestrictionMap(allPromotions: Seq[Promotion]) : Map[String,Set[String]] = {

        val promotionRestrictionMap = Map[String,Set[String]]()
        for (promotion <- allPromotions) {
            promotionRestrictionMap += promotion.code -> promotion.notCombinableWith.toSet
        }
        return promotionRestrictionMap
    }

    /** builds a set of promotion codes
     *
     *  @param allPromotions sequence of all promotions
     *  @return set of promotion codes
     */
    private def buildPromotionCodeSet(allPromotions: Seq[Promotion]) : Set[String] = {

       return buildPromotionCodeSet(allPromotions, Set[String]())
    }

    /** builds a set of promotion codes from all promotion codes excluding a set of promotion codes
     *
     *  @param allPromotions sequence of all promotions
     *  @param promotionsToExclude set of promotion codes to exclude
     *  @return set of promotion codes
     */
    private def buildPromotionCodeSet(allPromotions: Seq[Promotion], promotionsToExclude: Set[String]) : Set[String] = {

        var promotionCodes: Set[String] = Set[String]()
        for (promotion <- allPromotions) {
            promotionCodes += promotion.code
        }
        promotionCodes --= promotionsToExclude
        return promotionCodes
    }

    /** builds promotion code combinations
     *
     *  @param promotionsToInclude set of promotion codes to include in combinations
     *  @param promotionsToConsider set of promotion codes to consider in combinations
     *  @param promotionRestrictionMap map from promotion code to set of promotion codes that are not combinable with the promotion
     *  @return set of set of promotion codes that are allowed in combination
     */
    private def buildCombinations(promotionsToInclude: Set[String], promotionsToConsider: Set[String], promotionRestrictionMap: Map[String,Set[String]]) : Set[Set[String]] = {

        //
        //  if there are no promotions to consider
        //

        if (promotionsToConsider.isEmpty) {

            //
            //  consider the case that the promotions to include might already combine with other promotions that are not being considered
            //

            var othersInCombination: Set[String] = Set[String]()
            othersInCombination ++= promotionRestrictionMap.keySet
            othersInCombination --= promotionsToInclude
            for (promotionToInclude <- promotionsToInclude) {
                othersInCombination --= promotionRestrictionMap(promotionToInclude)
            }
            return if (othersInCombination.isEmpty) Set[Set[String]](promotionsToInclude) else Set[Set[String]]()
        }

        //
        //  consider each promotion to include in combination with remaining promotions to consider
        //

        var combinations: Set[Set[String]] = Set[Set[String]]()
        var remainingPromotionsToConsider : Set[String] = Set[String]()
        remainingPromotionsToConsider  ++= promotionsToConsider
        for (candidate <- promotionsToConsider) {
            remainingPromotionsToConsider  -= candidate
            var violations: Set[String] = promotionsToInclude.intersect(promotionRestrictionMap(candidate))
            if (violations.isEmpty) {
                combinations ++= buildCombinations(promotionsToInclude + candidate, remainingPromotionsToConsider , promotionRestrictionMap)
            }
        }
        return if (combinations.isEmpty) buildCombinations(promotionsToInclude, Set[String](), promotionRestrictionMap) else combinations
    }

    /** finds all promotion combinations with maximum number of combinable promotions in each
     *
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

        //
        //  build map from promotion code to set of promotion codes that are not combinable with the promotion
        //

        val promotionRestrictionMap = buildPromotionRestrictionMap(allPromotions)

        //
        //  build a set of candidate promotions to consider for combinations
        //

        val promotionsToConsider = buildPromotionCodeSet(allPromotions)

        //
        //  return sequence of promotion combinations
        //

        var seq : Seq[PromotionCombo] = Seq[PromotionCombo]()
        buildCombinations(Set[String](), promotionsToConsider, promotionRestrictionMap).foreach((s:Set[String]) => seq = seq :+ PromotionCombo(s.toSeq))
        return seq
    }

    /** finds all promotion combinations for a given promotion
     *
     *  @param promotionCode the promotion to include in all promotion combinations
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]) : Seq[PromotionCombo] = {

        //
        //  build map from promotion code to set of promotion codes that are not combinable with the promotion
        //

        val promotionRestrictionMap = buildPromotionRestrictionMap(allPromotions)

        //
        //  build a set of candidate promotions to consider for combinations, excluding the promotionCode parameter
        //

        val promotionsToConsider = buildPromotionCodeSet(allPromotions, Set[String](promotionCode))

        //
        //  return sequence of promotion combinations
        //

        var seq : Seq[PromotionCombo] = Seq[PromotionCombo]()
        buildCombinations(Set[String](promotionCode), promotionsToConsider, promotionRestrictionMap).foreach((s:Set[String]) => seq = seq :+ PromotionCombo(s.toSeq))
        return seq
  }
}
