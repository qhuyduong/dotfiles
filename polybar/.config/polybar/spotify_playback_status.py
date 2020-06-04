#!/usr/bin/env python3

import dbus

playback_status_icons = {
    "Paused": "",
    "Playing": ""
}

try:
    session_bus = dbus.SessionBus()
    spotify_bus = session_bus.get_object(
        'org.mpris.MediaPlayer2.spotify',
        '/org/mpris/MediaPlayer2'
    )

    spotify_properties = dbus.Interface(
        spotify_bus,
        'org.freedesktop.DBus.Properties'
    )

    playback_status = spotify_properties.Get('org.mpris.MediaPlayer2.Player', 'PlaybackStatus')

    print(playback_status_icons[playback_status])
except Exception as e:
    if isinstance(e, dbus.exceptions.DBusException):
        print('')
    else:
        print(e)
