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

        def buildLookupRateGroupFunction(rates: Seq[Rate]) : (String) => String = {

            def addRatesToRateGroupMap(rates: Seq[Rate], map: Map[String,String]) : Map[String,String] = {
                if (rates.isEmpty) map else addRatesToRateGroupMap(rates.tail, map + (rates.head.rateCode -> rates.head.rateGroup))
            }

            (rateCode: String) => addRatesToRateGroupMap(rates, Map[String,String]())(rateCode)
        }

        def addPriceToMap(price: CabinPrice, rateGroup: String, map: Map[String,Map[String,BestGroupPrice]]) : Map[String,Map[String,BestGroupPrice]] = {
            map get price.cabinCode match {
                case None => {
                    map + (price.cabinCode -> Map[String,BestGroupPrice](rateGroup -> BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateGroup)))
                }
                case Some(groupBestPriceMap) => {
                    groupBestPriceMap get rateGroup match {
                        case None => {
                            map + (price.cabinCode -> (groupBestPriceMap + (rateGroup -> BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateGroup))))
                        }
                        case Some(currentBestPrice) => {
                            if (currentBestPrice.price > price.price) map + (price.cabinCode -> (groupBestPriceMap + (rateGroup -> BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateGroup)))) else map
                        }
                    }
                }
            }
        }

        def addPricesToMap(prices: Seq[CabinPrice], map: Map[String,Map[String,BestGroupPrice]], lookupRateGroup: (String) => String) : Map[String,Map[String,BestGroupPrice]] = {
            if (prices.isEmpty) map else addPricesToMap(prices.tail, addPriceToMap(prices.head, lookupRateGroup(prices.head.rateCode), map), lookupRateGroup)
        }

        addPricesToMap(prices, Map[String,Map[String,BestGroupPrice]](), buildLookupRateGroupFunction(rates)).mapValues(x=>x.valuesIterator.toList).valuesIterator.toList.flatten   
    }

    private def buildCombinations(allPromotions: Seq[Promotion]) : Seq[PromotionCombo] = {
        buildCombinations(allPromotions, Set[String]())
    }

    private def buildCombinations(allPromotions: Seq[Promotion], promotionsToInclude: Set[String]) : Seq[PromotionCombo] = {

        def buildLookupPromotionRestrictionsFunction(allPromotions: Seq[Promotion]) : (String) => Set[String] = {

            def addPromotionsToPromotionRestrictionsMap(promotions: Seq[Promotion], map: Map[String,Set[String]]) : Map[String,Set[String]] = {
                if (promotions.isEmpty) map else addPromotionsToPromotionRestrictionsMap(promotions.tail, map + (promotions.head.code -> promotions.head.notCombinableWith.toSet))
            }

            (promotionCode: String) => addPromotionsToPromotionRestrictionsMap(allPromotions, Map[String,Set[String]]())(promotionCode)
        }

        def buildPromotionSets(
            allPromotions: Seq[Promotion],
            promotionsToInclude: Set[String],
            lookupPromotionRestrictions: (String) => Set[String]
        ): Set[Set[String]] = {

            def iterateCombinations(
                promotions: Set[String],
                restrictions: Set[String],
                remainingPromotions: Seq[String],
                allPromotions: Set[String],
                lookupPromotionRestrictions: (String) => Set[String]
            ): Set[Set[String]] = {

                def maximumSetFilter(
                    promotions: Set[String],
                    restrictions: Set[String],
                    allPromotions: Set[String]
                ): Set[Set[String]] = {

                    if (((allPromotions -- promotions) -- restrictions).isEmpty)

                        Set[Set[String]](promotions)

                    else

                        Set[Set[String]]()
                }

                def combine(
                    promotions: Set[String],
                    restrictions: Set[String],
                    promotion: String,
                    promotionRestrictions: Set[String],
                    remainingPromotions: Seq[String],
                    allPromotions: Set[String],
                    lookupPromotionRestrictions: (String) => Set[String]
                ): Set[Set[String]] = {

                    if ((promotions & promotionRestrictions).isEmpty)

                        iterateCombinations(
                            promotions + promotion,
                            restrictions ++ promotionRestrictions,
                            remainingPromotions,
                            allPromotions,
                            lookupPromotionRestrictions
                        )

                    else

                        maximumSetFilter(
                            promotions,
                            restrictions,
                            allPromotions
                        )

                }

                if (remainingPromotions.isEmpty)

                    maximumSetFilter(
                        promotions,
                        restrictions,
                        allPromotions
                    )

                else

                    combine(
                        promotions,
                        restrictions,
                        remainingPromotions.head,
                        lookupPromotionRestrictions(remainingPromotions.head),
                        remainingPromotions.tail,
                        allPromotions,
                        lookupPromotionRestrictions
                    ) ++
                    iterateCombinations(
                        promotions,
                        restrictions,
                        remainingPromotions.tail,
                        allPromotions,
                        lookupPromotionRestrictions
                    )
            }

            iterateCombinations(
                promotionsToInclude,
                promotionsToInclude.map(promotion => lookupPromotionRestrictions(promotion)).flatten,
                allPromotions.map(promotion=>promotion.code).filterNot(promotionsToInclude contains _),
                allPromotions.map(promotion=>promotion.code).toSet,
                lookupPromotionRestrictions
            )
        }

        buildPromotionSets(
            allPromotions,
            promotionsToInclude,
            buildLookupPromotionRestrictionsFunction(allPromotions)
       ).toSeq.map(s => PromotionCombo(s.toSeq))
    }

    /** finds all promotion combinations with maximum number of combinable promotions in each
     *
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

        buildCombinations(allPromotions)
    }

    /** finds all promotion combinations for a given promotion
     *
     *  @param promotionCode the promotion to include in all promotion combinations
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]) : Seq[PromotionCombo] = {

        buildCombinations(allPromotions, Set[String](promotionCode))
    }
}
