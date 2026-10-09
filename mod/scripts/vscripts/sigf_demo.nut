SigfShowcase("soldier")
::DemoTitan <- null

// A Titan is placed in the most open direction in front of the player.
::Summon <- function(dist) {
	local host = SigfHost()
	local t = ::DemoTitan
	if (t == null || !t.IsValid() || !t.IsAlive() || !::IsTitanPlayer(t)) t = ::SpawnTitan()
	if (t == null) {
		foreach (p in SigfPlayers()) if (p != host && p.GetTeam() == 3) { ::MakeTitan(p); t = p; break }
	}
	if (t == null) return null
	::DemoTitan = t
	local bestYaw = host.EyeAngles().y, bestFree = -1.0
	for (local i = 0; i < 12; i++) {
		local yaw = i * 30.0
		local r = yaw / 57.29578
		local from = host.GetOrigin() + Vector(0, 0, 60)
		local tr = { start = from, end = from + Vector(cos(r), sin(r), 0) * dist * 1.3, ignore = host }
		TraceLineEx(tr)
		local f = tr.hit ? tr.fraction : 1.0
		if (f > bestFree) { bestFree = f; bestYaw = yaw }
	}
	local r = bestYaw / 57.29578
	t.SetAbsOrigin(host.GetOrigin() + Vector(cos(r), sin(r), 0) * dist + Vector(0, 0, 10))
	t.SetAbsVelocity(Vector(0, 0, 0))
	host.SnapEyeAngles(QAngle(0, bestYaw, 0))
	t.SetHealth(::TitanHP + 150)
	return t
}

::AimTitan <- function(t) {
	local host = SigfHost()
	local d = (t.GetOrigin() + Vector(0, 0, 220)) - host.EyePosition()
	host.SnapEyeAngles(QAngle(-atan2(d.z, sqrt(d.x * d.x + d.y * d.y)) * 57.29578, atan2(d.y, d.x) * 57.29578, 0))
}

// Keeps the player's view on the Titan for a few seconds (the pilot waits).
::Track <- function(t, sec) {
	::SigfPilotPause <- Time() + sec + 0.5
	for (local i = 0; i < sec * 4; i++) SigfIn(i * 0.25, function() { if (t != null && t.IsValid() && t.IsAlive()) ::AimTitan(t) })
}
::Cam <- function() { SendToConsole("cam_idealdist 330; cam_idealdistup 80") }

SigfDemo(0.5, function() { ::Cam() })
SigfDemo(3, function() { ::Cam() })
SigfDemo(1, function() { SigfCaption("ATTACK ON TITAN TAKEOVER", 4); local t = ::Summon(650); if (t) ::Track(t, 6) })
SigfDemo(8, function() {
	SigfCaption("A Titan! Shoot it in the nape!", 4)
	local t = ::Summon(650)
	if (t) { ::Track(t, 8); SigfShoot(6.0) }
})
SigfDemo(19, function() {
	SigfCaption("ODM gear: grapple to the nape and slice!", 4)
	local t = ::Summon(650)
	if (t) { ::Track(t, 7); SigfIn(1.0, function() { ::OdmSwing(SigfHost(), t) }); SigfIn(4.0, function() { ::OdmSwing(SigfHost(), t) }) }
})
SigfDemo(31, function() {
	SigfCaption("The Survey Corps swarm the Titan", 4)
	local t = ::Summon(650)
	if (t) {
		::Track(t, 8)
		foreach (p in SigfPlayers()) if (p.GetTeam() == 2 && p != SigfHost()) {
			p.SetAbsOrigin(t.GetOrigin() + Vector(RandomFloat(-500, 500), RandomFloat(-500, 500), 40))
			SigfIn(RandomFloat(0.5, 3.0), function() { if (p.IsValid() && t.IsValid()) ::OdmSwing(p, t) })
		}
	}
})
SigfDemo(43, function() {
	SigfCaption("Two Titans at once!", 4)
	::TitanMax = 3
	local t = ::Summon(650)
	::SpawnTitan()
	if (t) ::Track(t, 8)
})
SigfDemo(55, function() {
	SigfCaption("Titan down: steam, shaking ground, a heal!", 5)
	local t = ::Summon(600)
	if (t) {
		t.SetHealth(120)
		::Track(t, 8)
		SigfShoot(3.0)
		SigfIn(1.5, function() { ::OdmSwing(SigfHost(), t) })
	}
})
