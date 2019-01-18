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

            def buildRateGroupMap(rates: Seq[Rate]) : Map[String,String] = {
               addRatesToRateGroupMap(rates, Map[String,String]())
            }

            (rateCode: String) => buildRateGroupMap(rates)(rateCode)
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

    private def buildCombinations(allPromotions: Seq[Promotion]) : Set[Set[String]] = {
        buildCombinations(allPromotions, Set[String]())
    }

    private def buildCombinations(allPromotions: Seq[Promotion], promotionsToInclude: Set[String]) : Set[Set[String]] = {

        def buildLookupPromotionRestrictionsFunction(allPromotions: Seq[Promotion]) : (String) => Set[String] = {

            def addPromotionsToPromotionRestrictionsMap(promotions: Seq[Promotion], map: Map[String,Set[String]]) : Map[String,Set[String]] = {
                if (promotions.isEmpty) map else addPromotionsToPromotionRestrictionsMap(promotions.tail, map + (promotions.head.code -> promotions.head.notCombinableWith.toSet))
            }

            def buildPromotionRestrictionsMap(allPromotions: Seq[Promotion]) : Map[String,Set[String]] = {
               addPromotionsToPromotionRestrictionsMap(allPromotions, Map[String,Set[String]]())
            }

            (promotionCode: String) => buildPromotionRestrictionsMap(allPromotions)(promotionCode)
        }

        def iterateCombinations(
            promotions: Set[String],
            restrictions: Set[String],
            remainingPromotions: Seq[String],
            allPromotions: Set[String],
            lookupPromotionRestrictions: (String) => Set[String]
        ): Set[Set[String]] = {

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

                else if (((allPromotions -- promotions) -- restrictions).isEmpty)

                    Set[Set[String]](promotions)

                else

                    Set[Set[String]]()
            }

            if (remainingPromotions.isEmpty) {

                if (((allPromotions -- promotions) -- restrictions).isEmpty)

                    Set[Set[String]](promotions)

                else

                    Set[Set[String]]()
            }
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

        val lookupPromotionRestrictions = buildLookupPromotionRestrictionsFunction(allPromotions)

        iterateCombinations(
            promotionsToInclude,
            promotionsToInclude.map(promotion => lookupPromotionRestrictions(promotion)).flatten,
            allPromotions.map(promotion=>promotion.code).filterNot(promotionsToInclude contains _),
            allPromotions.map(promotion=>promotion.code).toSet,
            lookupPromotionRestrictions
        )
    }

    /** finds all promotion combinations with maximum number of combinable promotions in each
     *
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

        buildCombinations(allPromotions).toSeq.map(s => PromotionCombo(s.toSeq))
    }

    /** finds all promotion combinations for a given promotion
     *
     *  @param promotionCode the promotion to include in all promotion combinations
     *  @param allPromotions sequence of all promotions
     *  @return sequence of promotion combinations
     */
    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]) : Seq[PromotionCombo] = {

        buildCombinations(allPromotions, Set[String](promotionCode)).toSeq.map(s => PromotionCombo(s.toSeq))
    }

}
