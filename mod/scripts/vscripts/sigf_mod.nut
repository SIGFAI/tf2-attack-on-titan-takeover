// Attack on Titan Takeover: BLU bots grow into Titans, RED bots are the Survey Corps with ODM gear.
PrecacheSound("mvm/giant_heavy/giant_heavy_step01.wav")
PrecacheSound("mvm/giant_heavy/giant_heavy_step02.wav")
PrecacheSound("mvm/giant_heavy/giant_heavy_step03.wav")
PrecacheSound("mvm/giant_common/giant_common_explodes_01.wav")
PrecacheSound("ambient/halloween/male_scream_15.wav")
PrecacheSound("ambient/halloween/male_scream_22.wav")
PrecacheSound("weapons/grappling_hook_shoot.wav")
PrecacheSound("weapons/samurai/tf_katana_slice_01.wav")
PrecacheSound("weapons/samurai/tf_katana_slice_02.wav")
PrecacheModel("sprites/laserbeam.vmt")

::TitanHP <- 4500
::TitanScale <- 5.0
::TitanMax <- 2
::TitanIds <- {}       // userid -> true
::NapeBusy <- false
::OdmAt <- {}          // userid -> next swing time

::Txt <- function(msg, y, sec, color) {
	local t = SpawnEntityFromTable("game_text", { message = msg, x = -1, y = y, effect = 0, color = color, color2 = "255 255 255",
		fadein = 0.05, fadeout = 0.4, holdtime = sec, fxtime = 0, channel = 2, spawnflags = 1 })
	EntFireByHandle(t, "Display", "", 0, null, null)
	EntFireByHandle(t, "Kill", "", sec + 1.0, null, null)
}

::Uid <- function(p) { return p.entindex() }
::IsTitanPlayer <- function(p) { return ::Uid(p) in ::TitanIds }

::Tint <- function(p, r, g, b) { NetProps.SetPropInt(p, "m_clrRender", r | (g << 8) | (b << 16) | (255 << 24)) }

::Reset <- function(p) {
	p.SetModelScale(1.0, 0.0)
	p.RemoveCustomAttribute("max health additive bonus")
	p.RemoveCustomAttribute("move speed bonus")
	p.RemoveCustomAttribute("damage bonus")
	p.RemoveCustomAttribute("increased jump height")
	p.RemoveCustomAttribute("cancel falling damage")
	::Tint(p, 255, 255, 255)
}

::Nape <- function(t) { return t.GetOrigin() + Vector(0, 0, 62 * ::TitanScale) }

::MakeTitan <- function(p) {
	::TitanIds[::Uid(p)] <- true
	p.AddCustomAttribute("max health additive bonus", ::TitanHP, -1)
	p.AddCustomAttribute("move speed bonus", 0.9, -1)
	p.AddCustomAttribute("damage bonus", 2.0, -1)
	p.SetHealth(::TitanHP + 150)
	p.SetModelScale(::TitanScale, 1.2)
	::Tint(p, 255, 215, 190)
	local g = SigfGround()
	if (g != null) { p.SetAbsOrigin(g + Vector(0, 0, 20)); p.SetAbsVelocity(Vector(0, 0, 0)) }
	local pos = p.GetOrigin()
	DispatchParticleEffect("ExplosionCore_MidAir", pos + Vector(0, 0, 120), Vector(0, 0, 0))
	ScreenShake(pos, 12.0, 150.0, 2.0, 2500.0, 0, true)
	EmitSoundEx({ sound_name = "ambient/halloween/male_scream_22.wav", origin = pos, sound_level = 130, pitch = 45 })
	::Txt("A TITAN APPEARS!", 0.2, 3.0, "255 80 40")
}

::Titans <- function() {
	local out = []
	foreach (p in SigfPlayers()) if (::IsTitanPlayer(p)) out.append(p)
	return out
}

::SpawnTitan <- function() {
	if (::Titans().len() >= ::TitanMax) return null
	local host = GetListenServerHost()
	local c = []
	foreach (p in SigfPlayers()) if (p != host && p.GetTeam() == 3 && !::IsTitanPlayer(p)) c.append(p)
	if (!c.len()) return null
	local p = c[RandomInt(0, c.len() - 1)]
	::MakeTitan(p)
	return p
}

// ODM gear swing: a grappling wire pulls the player to the Titan nape, then the blades cut.
::OdmSwing <- function(p, t) {
	if (p == null || t == null || !p.IsValid() || !t.IsValid() || !t.IsAlive()) return
	local from = p.GetOrigin() + Vector(0, 0, 40)
	local nape = ::Nape(t)
	local d = nape - from
	local dist = d.Length()
	d.Norm()
	p.SetAbsVelocity(d * (700.0 + dist * 1.2 > 1500.0 ? 1500.0 : 700.0 + dist * 1.2) + Vector(0, 0, 150))
	EmitSoundEx({ sound_name = "weapons/grappling_hook_shoot.wav", origin = from, sound_level = 85 })
	local name = "odm" + p.entindex()
	p.KeyValueFromString("targetname", name)
	local tgt = SpawnEntityFromTable("info_target", { targetname = name + "t", origin = nape })
	local beam = SpawnEntityFromTable("env_beam", { targetname = name + "b", LightningStart = name, LightningEnd = name + "t",
		texture = "sprites/laserbeam.vmt", BoltWidth = 3.0, life = 0, spawnflags = 1, rendercolor = "230 230 230", renderamt = 255,
		TouchType = 0, NoiseAmplitude = 0, framerate = 0, StrikeTime = 0, damage = 0, texturescroll = 35 })
	EntFireByHandle(beam, "Kill", "", 0.7, null, null)
	EntFireByHandle(tgt, "Kill", "", 0.8, null, null)
	SigfIn(0.55, function() {
		if (p == null || !p.IsValid() || !p.IsAlive() || t == null || !t.IsValid() || !t.IsAlive()) return
		if ((p.GetOrigin() - t.GetOrigin()).Length() > 420 + 60 * ::TitanScale) return
		::Slice(p, t)
	})
}

::Slice <- function(p, t) {
	local np = ::Nape(t)
	::NapeBusy = true
	t.TakeDamage(160.0, 4, p)
	::NapeBusy = false
	DispatchParticleEffect("blood_impact_red_01", np, Vector(0, 0, 0))
	DispatchParticleEffect("ExplosionCore_MidAir", np, Vector(0, 0, 0))
	EmitSoundEx({ sound_name = RandomInt(0, 1) ? "weapons/samurai/tf_katana_slice_01.wav" : "weapons/samurai/tf_katana_slice_02.wav", origin = np, sound_level = 95 })
}

::MyEvents <- {
	OnGameEvent_player_spawn = function(params) {
		local p = GetPlayerFromUserID(params.userid)
		if (p == null || p.GetTeam() < 2) return
		if (p.entindex() in ::TitanIds) delete ::TitanIds[p.entindex()]
		::Reset(p)
		if (p.GetTeam() == 2) {
			::Tint(p, 150, 225, 150)
			p.AddCustomAttribute("move speed bonus", 1.25, -1)
			p.AddCustomAttribute("increased jump height", 1.5, -1)
			p.AddCustomAttribute("cancel falling damage", 1, -1)
		}
	}
	OnGameEvent_player_hurt = function(params) {
		if (::NapeBusy) return
		local v = GetPlayerFromUserID(params.userid)
		local a = GetPlayerFromUserID(params.attacker)
		if (v == null || !(v.entindex() in ::TitanIds)) return
		local frac = v.GetHealth().tofloat() / (::TitanHP + 150)
		if (frac < 0) frac = 0.0
		if (frac > 1) frac = 1.0
		::Tint(v, 255, (60 + 140 * frac).tointeger(), (50 + 120 * frac).tointeger())
		DispatchParticleEffect("blood_impact_red_01", v.GetOrigin() + Vector(0, 0, 40 * ::TitanScale), Vector(0, 0, 0))
		if (a != null && a != v) {
			local to = a.GetOrigin() - v.GetOrigin()
			to.z = 0
			local fwd = v.EyeAngles().Forward()
			fwd.z = 0
			if (to.Dot(fwd) < 0 || a.GetOrigin().z > v.GetOrigin().z + 150) {
				::NapeBusy = true
				v.TakeDamage(params.damageamount * 1.0, 4, a)
				::NapeBusy = false
				DispatchParticleEffect("ExplosionCore_MidAir", ::Nape(v), Vector(0, 0, 0))
				EmitSoundEx({ sound_name = "weapons/samurai/tf_katana_slice_01.wav", origin = ::Nape(v), sound_level = 90 })
			}
		}
	}
	OnGameEvent_player_death = function(params) {
		local v = GetPlayerFromUserID(params.userid)
		if (v == null || !(v.entindex() in ::TitanIds)) return
		local pos = v.GetOrigin()
		delete ::TitanIds[v.entindex()]
		::Reset(v)
		for (local i = 0; i < 5; i++) {
			local o = Vector(RandomFloat(-120, 120), RandomFloat(-120, 120), RandomFloat(30, 260))
			SigfIn(i * 0.25, function() { DispatchParticleEffect("ExplosionCore_MidAir", pos + o, Vector(0, 0, 0)) })
		}
		ScreenShake(pos, 16.0, 150.0, 2.5, 2500.0, 0, true)
		EmitSoundEx({ sound_name = "mvm/giant_common/giant_common_explodes_01.wav", origin = pos, sound_level = 130 })
		EmitSoundEx({ sound_name = "ambient/halloween/male_scream_15.wav", origin = pos, sound_level = 120, pitch = 50 })
		::Txt("TITAN SLAIN!  Humanity advances.", 0.2, 3.0, "120 255 120")
		local k = GetPlayerFromUserID(params.attacker)
		if (k != null && k.IsValid() && k.IsAlive()) k.SetHealth(k.GetMaxHealth())
	}
}
__CollectGameEventCallbacks(::MyEvents)

// Titans stomp: dust, shaking ground, and anyone close gets thrown.
SigfEvery(0.9, function() {
	foreach (t in ::Titans()) {
		if (t.GetAbsVelocity().Length() < 60) continue
		local pos = t.GetOrigin()
		if (RandomInt(0, 7) == 0) EmitSoundEx({ sound_name = "ambient/halloween/male_scream_22.wav", origin = pos, sound_level = 125, pitch = 45 })
		ScreenShake(pos, 6.0, 40.0, 0.6, 1400.0, 0, true)
		EmitSoundEx({ sound_name = "mvm/giant_heavy/giant_heavy_step0" + RandomInt(1, 3) + ".wav", origin = pos, sound_level = 120 })
		DispatchParticleEffect("ExplosionCore_wall", pos, Vector(0, 0, 0))
		foreach (p in SigfNear(pos, 230)) {
			if (p == t || p == GetListenServerHost() || ::IsTitanPlayer(p)) continue
			local d = p.GetOrigin() - pos
			d.z = 0
			d.Norm()
			p.SetAbsVelocity(d * 300 + Vector(0, 0, 250))
		}
	}
})

// Survey Corps bots fire their ODM gear at nearby Titans.
SigfEvery(1.0, function() {
	local now = Time()
	local ts = ::Titans()
	if (!ts.len()) return
	foreach (p in SigfPlayers()) {
		if (p.GetTeam() != 2 || p == GetListenServerHost()) continue
		local id = ::Uid(p)
		if ((id in ::OdmAt) && ::OdmAt[id] > now) continue
		local best = null, bd = 1800.0
		foreach (t in ts) { local d = (t.GetOrigin() - p.GetOrigin()).Length(); if (d < bd) { bd = d; best = t } }
		if (best == null || bd < 120) continue
		::OdmAt[id] <- now + RandomFloat(7.0, 10.0)
		::OdmSwing(p, best)
	}
})

SigfAfter(3.0, function() { ::SpawnTitan() })
SigfEvery(12.0, function() { ::SpawnTitan() })
// A Titan that gets wedged in a doorway is carried to open ground; healers cannot push it past its max health.
::TitanPos <- {}
SigfEvery(2.0, function() {
	foreach (t in ::Titans()) {
		local id = t.entindex()
		local pos = t.GetOrigin()
		if (t.GetHealth() > ::TitanHP + 150) t.SetHealth(::TitanHP + 150)
		if ((id in ::TitanPos) && (::TitanPos[id] - pos).Length() < 25) {
			local fighting = false
			foreach (p in SigfNear(pos, 350)) if (p.GetTeam() == 2) fighting = true
			local g = SigfGround()
			if (!fighting && g != null) { t.SetAbsOrigin(g + Vector(0, 0, 20)); t.SetAbsVelocity(Vector(0, 0, 0)) }
		}
		::TitanPos[id] <- pos
	}
})
