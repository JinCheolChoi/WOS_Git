#********************
#
# empty the workspace
#
#********************
rm(list=ls())

library(data.table)
library(dplyr)

# import
user_profile=Sys.getenv("USERPROFILE")
Troop_info=fread(paste0(user_profile, "/Desktop/Troop_info.csv"))
# colClasses="character")

# Lanchester_Simulation
# lanchester's square decrete version
Lanchester_Simulation=function(R.attack,
                               R.defense,
                               R.health,
                               R.lethality,
                               R.n_of_troops,
                               D.attack,
                               D.defense,
                               D.health,
                               D.lethality,
                               D.n_of_troops){
  # Damage_per_hit
  Damage_per_hit_calculator=function(Adjusted_ally_attack,
                                     Adjusted_enemy_defense,
                                     Adjusted_enemy_health,
                                     Adjusted_ally_lethality){
    Damage_per_hit=Adjusted_ally_attack*Adjusted_ally_lethality/Adjusted_enemy_defense
    return(Damage_per_hit)
  }
  
  # Total_damage
  Total_damage_at_t=function(Damage_per_hit,
                             Number_of_troops){
    Total_damage=Damage_per_hit*ceiling(sqrt(Number_of_troops))
    return(Total_damage)
  }
  
  #
  Death_at_t=function(Ally_total_damage,
                      Enemy_health){
    Death=Ally_total_damage/(Enemy_health)
    return(Death)
  }
  # R.attack=3
  # R.defense=6
  # R.health=8
  # R.lethality=3
  # R.n_of_troops=100
  # D.attack=3
  # D.defense=6
  # D.health=8
  # D.lethality=3
  # D.n_of_troops=500
  
  
  # record turn
  Turn=0
  
  # record initial troop numbers
  R.n_of_troops_at_t=c(R.n_of_troops)
  D.n_of_troops_at_t=c(D.n_of_troops)
  
  while(tail(R.n_of_troops_at_t, 1)>0.001 &
        tail(D.n_of_troops_at_t, 1)>0.001){
    # Turn
    Turn=c(Turn, tail(Turn, 1)+1)
    
    # damage per hit
    R.damage_per_hit=Damage_per_hit_calculator(R.attack,
                                               R.defense,
                                               D.health,
                                               D.lethality)
    D.damage_per_hit=Damage_per_hit_calculator(D.attack,
                                               D.defense,
                                               R.health,
                                               R.lethality)
    
    # total damage
    R.total_damage_at_t=Total_damage_at_t(R.damage_per_hit,
                                          R.n_of_troops_at_t[tail(Turn, 1)])
    D.total_damage_at_t=Total_damage_at_t(D.damage_per_hit,
                                          D.n_of_troops_at_t[tail(Turn, 1)])
    
    # death
    R.death_at_t=Death_at_t(D.total_damage_at_t,
                            R.health)
    D.death_at_t=Death_at_t(R.total_damage_at_t,
                            D.health)
    
    # R.death_at_t=(D.attack*D.lethality)/(R.defense*R.health)*(D.n_of_troops_at_t[tail(Turn, 1)]^(1/2))
    # D.death_at_t=(R.attack*R.lethality)/(D.defense*D.health)*(R.n_of_troops_at_t[tail(Turn, 1)]^(1/2))
    
    R.n_of_troops_at_t=c(R.n_of_troops_at_t, R.n_of_troops_at_t[tail(Turn, 1)]-R.death_at_t)
    D.n_of_troops_at_t=c(D.n_of_troops_at_t, D.n_of_troops_at_t[tail(Turn, 1)]-D.death_at_t)
  }
  
  # output
  output=data.table(
    Turn=Turn+1,
    R.n_of_troops_at_t=R.n_of_troops_at_t,
    D.n_of_troops_at_t=D.n_of_troops_at_t
  )
  
  return(output)
}



#
R.info=Troop_info[`Troop type`=="Infantry" &
                    Tier=="3",]
R.info$R.n_of_troops=500
D.info=Troop_info[`Troop type`=="Infantry" &
                    Tier=="3",]
D.info$D.n_of_troops=500


Lanchester_Simulation(
  R.attack=R.info$Attack,
  R.defense=R.info$Defense,
  R.health=R.info$Health,
  R.lethality=R.info$Lethality,
  R.n_of_troops=R.info$R.n_of_troops,
  
  D.attack=D.info$Attack,
  D.defense=D.info$Defense,
  D.health=D.info$Health,
  D.lethality=D.info$Lethality,
  D.n_of_troops=D.info$D.n_of_troops
)


Troop_info[,
           log(1+Attack)+log(1+Defense)+log(1+Health)+log(1+Lethality)]

