library(tidyverse)
library(plotly)


# Perk pool for Mindbender's Ambition

perkPool = list(
      barrel = c("Rifled Barrel", "Smoothbore", "Smallbore", "Barrel Shroud", "Corkscrew Rifling", "Full Choke"),
      magazine = c("Assault Mag", "Appended Mag", "Tactical Mag", "Extended Mag", "Steady Rounds", "Accurized Rounds", "Light Mag"),
      slot1 = c("Slideshot", "Threat Detector", "Snapshot Sights", "Pulse Monitor"),
      slot2 = c("Rampage", "Quickdraw", "Moving Target", "Auto-loading Holster"),
      masterwork = c("Range", "Stability", "Handling", "Reload Speed")
)


# Probability of dropping a specific target roll

rollChance = function(perkPool, dropChance = NULL) {
      slotChances = map_dbl(perkPool, length)
      prob = 1 / prod(slotChances)
      if (! is.null(dropChance)) {
            return(1 / prob * dropChance)
      } else {
            return(prob)
      }
} 

rollChance(perkPool) # spoiler alert it's small


# Target roll (specific)

target = list(
      barrel = "Rifled Barrel",
      magazine = "Accurized Rounds",
      slot1 = "Slideshot",
      slot2 = "Quickdraw",
      masterwork = "Range"
)

# Target roll (multiple options/relaxed)

target2 = list(
      barrel = c("Rifled Barrel", "Full Choke"),
      magazine = c("Accurized Rounds", "Assault Mag"),
      slot1 = c(),
      slot2 = c("Quickdraw"),
      masterwork = c()
)


# Get a random roll

sampleDrop = function(perkPool, n = 10) {
      1:n %>%
            map_df(function(rep) {
                  perkPool %>%
                        map(function(slot) {
                              sample(x = slot, size = 1, replace = TRUE)
                        })
            })
}


# 10,000 random rolls

rolls = sampleDrop(perkPool = perkPool, n = 10000)


# Check whether a roll has the targeted perks

checkPerks = function(roll, target) {
      names(target) %>%
            map_lgl(function(slot) {
                  if (is.null(target[[slot]])) TRUE else roll[[slot]] %in% target[[slot]]
            }) %>%
            all
}


# Sample drops until a target roll is found

dropRate_current = function(perkPool, target) {
      rolls = sampleDrop(perkPool, 1)
      targetDropped = checkPerks(rolls, target)
      while (targetDropped == FALSE) {
            newRoll = sampleDrop(perkPool, 1)
            targetDropped = checkPerks(newRoll, target)
            rolls = bind_rows(rolls, newRoll)
      }
      return(list(rolls = rolls, drops = nrow(rolls)))
}



# Proposed system ==============================================================================================================


# Get collected perks

perkCollection = function(rolls) {
      rolls %>%
            map(function(slot) {
                  unique(slot)
            })
}


# Check collected perks against a target roll

checkPerkCollection = function(collection, target) {
      names(target) %>%
            map_lgl(function(slot) {
                  if (is.null(target[[slot]])) TRUE else any(target[[slot]] %in% collection[[slot]])
            }) %>%
            all
}


# Sample drops until all target perks are in the collection

dropRate_proposed = function(perkPool, target) {
      rolls = sampleDrop(perkPool, 1)
      collection = perkCollection(rolls)
      targetDropped = checkPerkCollection(collection, target)
      while (targetDropped == FALSE) {
            rolls = bind_rows(rolls, sampleDrop(perkPool, 1))
            collection = perkCollection(rolls)
            targetDropped = checkPerkCollection(collection, target)
      }
      return(list(rolls = rolls, collection = collection, drops = nrow(rolls)))
}



# Simulate finding a target drop repeatedly using either system ===================================================

getDrops = function(type = "current", perkPool, target, n = 100, includeRolls = FALSE) {
      1:n %>%
            map(function(x) {
                  message(x, "/", n)
                  if (type == "current") {
                        result = dropRate_current(perkPool, target)
                  } else if (type == "proposed") {
                        result = dropRate_proposed(perkPool, target)
                  } else {
                        stop("invalid type")
                  }
                  if (includeRolls) return(result) else return(result$drops)
            })
}


dropsProposed = getDrops(type = "proposed", perkPool = perkPool, target = target, n = 1000)
dropsProposed_relaxed = getDrops(type = "proposed", perkPool = perkPool, target = target2, n = 1000)
dropsCurrent = getDrops(type = "current", perkPool = perkPool, target = target, n = 1000)
dropsCurrent_relaxed = getDrops(type = "current", perkPool = perkPool, target = target2, n = 1000)

df = data.frame(
      proposed = unlist(dropsProposed),
      proposed_relaxed = unlist(dropsProposed_relaxed),
      current = unlist(dropsCurrent),
      current_relaxed = unlist(dropsCurrent_relaxed),
      stringsAsFactors = FALSE
)

write_csv(x = df, path = "dropsim1000.csv")

p = df %>%
      gather(key = "system", value = "drops") %>%
      ggplot +
      geom_histogram(aes(x = drops, fill = system), bins = 500, alpha = 0.8) +
      theme_minimal()

ggplotly(p)


results = df %>%
      gather(key = "system", value = "drop") %>%
      table %>%
      prop.table(margin = 1) %>%
      data.frame %>%
      mutate(group = cut(x = as.numeric(drop), breaks = c(1, 5, 10, 20, 50, 100, 100000000), labels = c("1-5", "6-10", "11-20", "21-50", "50-100", "101+"), include.lowest = TRUE)) %>%
      group_by(system, group) %>%
      summarise(Freq = sum(Freq)) %>%
      mutate(Percentage = round(Freq * 100, 1))

results 
      


df %>%
      gather(key = "system", value = "drops") %>%
      ggplot +
      geom_histogram(aes(x = drops), bins = 50, alpha = 0.8) +
      facet_wrap(~system, scales = "free") +
      theme_minimal()














