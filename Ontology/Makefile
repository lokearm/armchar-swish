TTL=arm.ttl resources.ttl 
all: $(TTL)
install: $(TTL)
	cp $(TTL) ../serverdata/
R=Resources/resources.include Resources/vf.include Resources/abilities.include Resources/personality.include Resources/grimoire.include Resources/weapons.include
A=ArM/arm.include ArM/trait.include ArM/advancement.include ArM/spell.include ArM/equipment.include  ArM/character.include
L=Rules/logic.r Rules/advancement.r Rules/age.r Rules/castingscore.r Rules/traitinherit.r Rules/equipment.r

arm.ttl: prefix.ttl $A
	cat $^ > $@
resources.ttl: prefix.ttl $R
	cat $^ > $@

%.ttl: prefix.ttl %.include
	cat $^ > $@

clean:
	-rm $(TTL)

