# Salvo_Simulation
# Salvo's square decrete version
Salvo_Simulation=function(R.attack,
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
    Damage_per_hit=Adjusted_ally_attack*Adjusted_ally_lethality
    return(Damage_per_hit)
  }
  
  # Total_damage
  Total_damage_at_t=function(Damage_per_hit,
                             Number_of_troops){
    Total_damage=Damage_per_hit*(Number_of_troops)
    return(Total_damage)
  }
  
  # Defense_per_hit
  Defense_per_hit_calculator=function(Adjusted_ally_attack,
                                      Adjusted_enemy_defense,
                                      Adjusted_enemy_health,
                                      Adjusted_ally_lethality){
    Defense_per_hit=Adjusted_enemy_defense
    return(Defense_per_hit)
  }
  
  # Total_defense
  Total_defense_at_t=function(Defense_per_hit,
                              Number_of_troops){
    Total_defense=Defense_per_hit*(Number_of_troops)
    return(Total_defense)
  }
  
  #
  Death_at_t=function(Ally_total_damage,
                      Enemy_total_defense,
                      Enemy_health){
    Death=max(0, Ally_total_damage-Enemy_total_defense)/(Enemy_health)
    return(Death)
  }
  
  # R.attack=R.info$Attack
  # R.defense=R.info$Defense
  # R.health=R.info$Health
  # R.lethality=R.info$Lethality
  # R.n_of_troops=R.info$R.n_of_troops
  # 
  # D.attack=D.info$Attack
  # D.defense=D.info$Defense
  # D.health=D.info$Health
  # D.lethality=D.info$Lethality
  # D.n_of_troops=D.info$D.n_of_troops

  # record turn
  Turn=0
  
  # record initial troop numbers
  R.n_of_troops_at_t=c(R.n_of_troops)
  D.n_of_troops_at_t=c(D.n_of_troops)
  
  while(tail(R.n_of_troops_at_t, 1)>0.001 &
        tail(D.n_of_troops_at_t, 1)>0.001){
    # Turn
    Turn=c(Turn, tail(Turn, 1)+1)
    
    # # damage per hit
    # R.damage_per_hit=Damage_per_hit_calculator(R.attack,
    #                                            R.defense,
    #                                            D.health,
    #                                            D.lethality)
    # D.damage_per_hit=Damage_per_hit_calculator(D.attack,
    #                                            D.defense,
    #                                            R.health,
    #                                            R.lethality)
    # 
    # # total damage
    # R.total_damage_at_t=Total_damage_at_t(R.damage_per_hit,
    #                                       R.n_of_troops_at_t[tail(Turn, 1)])
    # D.total_damage_at_t=Total_damage_at_t(D.damage_per_hit,
    #                                       D.n_of_troops_at_t[tail(Turn, 1)])
    # 
    # # defense per hit
    # R.defense_per_hit=Defense_per_hit_calculator(R.attack,
    #                                              R.defense,
    #                                              D.health,
    #                                              D.lethality)
    # D.defense_per_hit=Defense_per_hit_calculator(D.attack,
    #                                              D.defense,
    #                                              R.health,
    #                                              R.lethality)
    # 
    # # total defense
    # R.total_defense_at_t=Total_defense_at_t(R.defense_per_hit,
    #                                         R.n_of_troops_at_t[tail(Turn, 1)])
    # D.total_defense_at_t=Total_defense_at_t(D.defense_per_hit,
    #                                         D.n_of_troops_at_t[tail(Turn, 1)])
    # 
    # # death
    # R.death_at_t=Death_at_t(D.total_damage_at_t,
    #                         R.total_defense_at_t,
    #                         R.health)
    # D.death_at_t=Death_at_t(R.total_damage_at_t,
    #                         D.total_defense_at_t,
    #                         D.health)
    
    # R.death_at_t=max(0,
    #                  (D.attack*D.n_of_troops_at_t[tail(Turn, 1)]-
    #                     R.defense*R.n_of_troops_at_t[tail(Turn, 1)]))/(R.health)
    # D.death_at_t=max(0,
    #                  (R.attack*R.n_of_troops_at_t[tail(Turn, 1)]-
    #                     D.defense*D.n_of_troops_at_t[tail(Turn, 1)]))/(D.health)
    
    # R.death_at_t=(D.attack*D.lethality)/(R.defense*R.health)*(D.n_of_troops_at_t[tail(Turn, 1)])
    # D.death_at_t=(R.attack*R.lethality)/(D.defense*D.health)*(R.n_of_troops_at_t[tail(Turn, 1)])
    
    # R.death_at_t=(D.attack*D.lethality-R.defense)/(R.health)*(D.n_of_troops_at_t[tail(Turn, 1)]/R.n_of_troops_at_t[tail(Turn, 1)])
    # D.death_at_t=(R.attack*R.lethality-D.defense)/(D.health)*(R.n_of_troops_at_t[tail(Turn, 1)]/D.n_of_troops_at_t[tail(Turn, 1)])
    
    R.death_at_t=(D.attack*D.lethality)/(R.defense*R.health)*(round(D.n_of_troops_at_t[tail(Turn, 1)]^(1/10)))
    D.death_at_t=(R.attack*R.lethality)/(D.defense*D.health)*(round(R.n_of_troops_at_t[tail(Turn, 1)]^(1/10)))
    
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
R.info=Troop_info[`Troop type`=="Marksman" &
                    Tier=="3",]
R.info$R.n_of_troops=200
D.info=Troop_info[`Troop type`=="Marksman" &
                    Tier=="3",]
D.info$D.n_of_troops=500

Salvo_Simulation(
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
