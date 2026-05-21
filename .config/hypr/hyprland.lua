--------------------------------------------------------------------------------
-- Setup utilities -------------------------------------------------------------
--------------------------------------------------------------------------------

local MOD = "SUPER"
local LS  = "SHIFT"
local MS1 = "mouse:272"
local MS2 = "mouse:273"
local RET = "Return"
local TAB = "Tab"
local PRD = "Period"
local CMM = "Comma"
local SPC = "Space"
local LFT = "Left"
local RGT = "Right"
local IPC = "qs -c noctalia-shell ipc call"

local function kbd(parts)
  return table.concat(parts, " + ")
end

--------------------------------------------------------------------------------
-- Startup ---------------------------------------------------------------------
--------------------------------------------------------------------------------

hl.on("hyprland.start", function()
  hl.exec_cmd([[tmux setenv -g HYPRLAND_INSTANCE_SIGNATURE "$HYPRLAND_INSTANCE_SIGNATURE"]])
  hl.exec_cmd("qs -c noctalia-shell")
  -- hl.exec_cmd("blueman-applet")
  -- hl.exec_cmd("nm-applet --indicator")
  -- hl.exec_cmd("redshift-gtk")
  -- hl.exec_cmd("sleep 1 && fish -c random_wallpaper") TODO qs integration?
end)

-- Ensure windows spawned by scratch terminal are moved into real workspace
hl.on("window.open", function(window)
    local active_ws = hl.get_active_workspace()
    local special_ws = hl.get_active_special_workspace()

    if not window or not active_ws or not special_ws then
      return
    end

    hl.dispatch(hl.dsp.focus({ window = window, follow = false }))
    hl.dispatch(hl.dsp.window.move({ workspace = active_ws.id, follow = false }))
end)

-- hl.env("HYPRCURSOR_THEME", "Nordzy-hyprcursors-white") TODO
-- hl.env("HYPRCURSOR_SIZE", "22")
-- hl.env("XCURSOR_PATH", "~/.icons:~/.local/share/icons:/usr/share/icons")
-- hl.env("XCURSOR_THEME", "Nordzy-cursors-white")
-- hl.env("XCURSOR_SIZE", "22")

--------------------------------------------------------------------------------
-- Core configuration ----------------------------------------------------------
--------------------------------------------------------------------------------

--hl.monitor({
--   output = "",
--   mode = "preferred",
--   position = "auto",
--   scale = "auto",
-- })
hl.bind("SUPER + SHIFT + M", function()
           hl.monitor({
                 output = "eDP-1",
                 disabled = true,
           })
end)

hl.config({
  debug = {
    disable_logs = false,
    enable_stdout_logs = true
  },
  general = {
    gaps_in = 4,
    gaps_out = 8,
    border_size = 4,
    resize_on_border = true,
    hover_icon_on_border = true,
    allow_tearing = false,
    col = {
      active_border = "rgb(6699CC)",
      inactive_border = "rgb(111111)",
    },
    layout = "dwindle"
  },

  ecosystem = {
    enforce_permissions = true,
  },

  input = {
    kb_layout = "de",
    follow_mouse = 1,
    kb_options = "ctrl:nocaps",
    special_fallthrough = true,
    sensitivity = 0,
    touchpad = {
      natural_scroll = false,
      disable_while_typing = false,
      middle_button_emulation = false,
    },
  },

  decoration = {
    dim_special = 0,
    dim_modal = 0,
    dim_inactive = 0,
    rounding = 0,
    active_opacity = 1.0,
    inactive_opacity = 1.0,
    fullscreen_opacity = 1.0,
    shadow = {
      enabled = true,
      range = 30,
      render_power = 3,
      color = "rgba(00000099)",
    },
  },

  group = {
    groupbar = {
      enabled = true,
      font_size = 12,
      font_weight_active = "bold",
      font_weight_inactive = "bold",
      gradients = true,
      height = 16,
      indicator_gap = -1,
      indicator_height = 0,
      stacked = false,
      render_titles = true,
      scrolling = true,
      text_color = "rgb(000000)",
      text_color_inactive = "rgb(999999)",
      col = {
        active = "rgb(4477AA)",
        inactive = "rgb(19191D)",
      },
      gaps_in = 0,
      gaps_out = 0,
      blur = true,
    },
  },

  misc = {
    force_default_wallpaper = 1,
    disable_hyprland_logo = true,
    font_family = "Fantasque Sans Mono",
    anr_missed_pings = 15,
  },

  binds = {
    workspace_back_and_forth = true,
  },

  dwindle = {
    force_split = 2,
    preserve_split = true,
    smart_resizing = true,
  },

  scrolling = {
    column_width = 1.0,
    wrap_focus = true
  }
})

-- hl.permission("/usr/(bin|local/bin)/grim", "screencopy", "allow")
-- hl.permission("/usr/(lib|libexec|lib64)/xdg-desktop-portal-hyprland", "screencopy", "allow")
-- hl.permission("/usr/(bin|local/bin)/hyprpm", "plugin", "allow")

--------------------------------------------------------------------------------
-- Rules -----------------------------------------------------------------------
--------------------------------------------------------------------------------

hl.window_rule({
  name = "Floating Windows Round Corners",
  match = { float = true },
  rounding = 8,
  rounding_power = 10,
})

-- hl.window_rule({
--   name = "Shadows Only Floating Windows",
--   match = { float = false },
--   no_shadow = true,
-- })

hl.window_rule({
  name = "Assign MPV to WS1",
  match = { class = "mpv" },
  workspace = 1,
})

hl.window_rule({
  name = "Assign Firefox to WS2",
  match = { class = "firefox" },
  workspace = 2,
})

hl.window_rule({
  name = "Assign Emacs to WS3",
  match = { class = "emacs" },
  workspace = 3,
})

hl.layer_rule({
  name = "Rofi Popup Animation",
  match = { class = "rofi" },
  animation = "popin 80%",
})

-- TODO
hl.animation({ leaf = "specialWorkspaceIn",  enabled = true, speed = 8, bezier = "default", style = "slide left" })
hl.animation({ leaf = "specialWorkspaceOut", enabled = true, speed = 8, bezier = "default", style = "slide left" })

-- TODO
-- hl.layer_rule({
--   name = "noctalia",
--   match = { namespace = "noctalia-background-.*$" },
--   ignore_alpha = 0.5,
--   blur = true,
--   blur_popups = true,
-- })

-- hl.window_rule({
--   name = "noctalia2",
--   match = { namespace = "noctalia-background-.*$" },
--   no_shadow = true,
-- })

-- Smart gaps
hl.workspace_rule({ workspace = "w[tv1]", gaps_out = 0, gaps_in = 0 })
hl.workspace_rule({ workspace = "f[1]",   gaps_out = 0, gaps_in = 0 })

hl.window_rule({ match = { float = false, workspace = "w[tv1]" }, border_size = 0, rounding = 0 })
hl.window_rule({ match = { float = false, workspace = "f[1]"   }, border_size = 0, rounding = 0 })

--------------------------------------------------------------------------------
-- Key binds -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Mouse
hl.bind(kbd({ MOD, MS1 }), hl.dsp.window.drag(),   { mouse = true })
hl.bind(kbd({ MOD, MS2 }), hl.dsp.window.resize(), { mouse = true })

-- Applications TODO
hl.bind(kbd({ MOD, LS, "E" }), hl.dsp.exit())
hl.bind(kbd({ MOD, RET     }), hl.dsp.exec_cmd("alacritty"))
hl.bind(kbd({ MOD, TAB     }), hl.dsp.exec_cmd("rofi -show drun"))
hl.bind(kbd({ MOD, "L"     }), hl.dsp.exec_cmd("rofi -show window"))
hl.bind(kbd({ MOD, "I"     }), hl.dsp.workspace.toggle_special("magic"))
hl.bind(kbd({ MOD, LS, "I" }), hl.dsp.window.move({ workspace = "special:magic" }))
hl.bind(kbd({ MOD, LS, PRD }), hl.dsp.exec_cmd("fish -c rofi_utils"))
hl.bind(kbd({ MOD, LS, "P" }), hl.dsp.exec_cmd("fish -c rofi_pass"))
hl.bind(kbd({ MOD, LS, "O" }), hl.dsp.exec_cmd("fish -c yequake"))
hl.bind(kbd({ MOD, LS, "R" }), hl.dsp.exec_cmd("hyprctl reload"))
hl.bind(kbd({ MOD, CMM     }), hl.dsp.exec_cmd(IPC .. " settings toggle"))
hl.bind(kbd({ MOD, "P"     }), hl.dsp.exec_cmd(IPC .. " controlCenter toggle"))
hl.bind(kbd({ MOD, LS, "X" }), hl.dsp.exec_cmd(IPC .. " lockScreen lock"))

-- Media / hardware keys TODO
-- hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("amixer -D pulse sset Master 5%+"), { repeating = true })
-- hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("amixer -D pulse sset Master 5%-"), { repeating = true })
-- hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("fish -c 'set_brightness +1000'"), { repeating = true })
-- hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("fish -c 'set_brightness 1000-'"), { repeating = true })

-- -- Locked screenshot binds TODO
-- hl.bind("Print", hl.dsp.exec_cmd([[sh -lc 'sleep 3; grim "$HOME/Pictures/screenshot-$(date +%F_%H-%M-%S).png" && notify-send "Screenshot taken" --icon=image']]), { locked = true })
-- hl.bind(kbd({ LS, "Print" }), hl.dsp.exec_cmd([[sh -lc 'grim -g "$(slurp)" "$HOME/Pictures/screenshot-$(date +%F_%H-%M-%S).png" && notify-send "Screenshot taken" --icon=image']]), { locked = true })

-- Window / layout
local function focus_group_aware(direction)
  local win = hl.get_active_window()

  if not win then
    hl.dispatch(hl.dsp.focus({ direction = direction }))
    return
  end

  local group = win.group

  if group then
    if direction == "left" then
      if group.current_index > 1 then
        hl.dispatch(hl.dsp.group.prev())
        return
      end
    elseif direction == "right" then
      if group.current_index < group.size then
        hl.dispatch(hl.dsp.group.next())
        return
      end
    end
  end

  hl.dispatch(hl.dsp.focus({ direction = direction }))
end

hl.bind(kbd({ MOD, "W" }), hl.dsp.focus({ direction = "up" }))
hl.bind(kbd({ MOD, "S" }), hl.dsp.focus({ direction = "down" }))
hl.bind(kbd({ MOD, "A" }), function() focus_group_aware("left") end)
hl.bind(kbd({ MOD, "D" }), function() focus_group_aware("right") end)
hl.bind(kbd({ MOD, "Q" }), hl.dsp.window.close())
hl.bind(kbd({ MOD, "F" }), hl.dsp.window.fullscreen({ action = "toggle" }))
hl.bind(kbd({ MOD, SPC }), hl.dsp.window.cycle_next({ next = true }))
hl.bind(kbd({ MOD, LS, SPC }), hl.dsp.window.float({ action = "toggle" }))
hl.bind(kbd({ MOD, LS, "I" }), hl.dsp.window.pin())
hl.bind(kbd({ MOD, "C" }), hl.dsp.group.toggle(hl.get_active_window()))
hl.bind(kbd({ MOD, "E" }), hl.dsp.layout("togglesplit"))

-- Workspaces
for i = 1, 10 do
  local keynum = i % 10 -- key 0 = workspace 10
  hl.bind(kbd({ MOD, tostring(keynum) }), hl.dsp.focus({ workspace = i }))
  hl.bind(kbd({ MOD, LS, tostring(keynum) }), hl.dsp.window.move({ workspace = i }))
end

hl.bind(kbd({ MOD, LS, "H" }), hl.dsp.workspace.move({ monitor = "-1" }))
hl.bind(kbd({ MOD, LS, LFT }), hl.dsp.workspace.move({ monitor = "-1" }))
hl.bind(kbd({ MOD, LS, "L" }), hl.dsp.workspace.move({ monitor = "+1" }))
hl.bind(kbd({ MOD, LS, RGT }), hl.dsp.workspace.move({ monitor = "+1" }))

--
-- -- Resize submap -------------------------------------------------------------
-- hl.bind(kbd({ MOD, "R" }), hl.dsp.submap("resize"))
-- hl.define_submap("resize", function()
--   hl.bind("h", hl.dsp.window.resize({ x = -10, y = 0, relative = true }))
--   hl.bind("l", hl.dsp.window.resize({ x = 10, y = 0, relative = true }))
--   hl.bind("k", hl.dsp.window.resize({ x = 0, y = -10, relative = true }))
--   hl.bind("j", hl.dsp.window.resize({ x = 0, y = 10, relative = true }))
--   hl.bind("Return", hl.dsp.submap("reset"))
--   hl.bind("Escape", hl.dsp.submap("reset"))
-- end)
--

local function log(msg)
  local file = io.open("/home/am/Documents/debug.txt", "a")
  if not file then
    return
  end
  file:write(tostring(msg) .. "\n")
  file:close()
end


-- hl.bind(kbd({ MOD, "X" }), function()
--   local windows = hl.get_windows({ tag = SCRATCH })
--
--   if not windows or #windows == 0 then
--     log("Found no scratch")
--     return
--   end
--
--   local scratch = windows[1]
--   local scratch_ws = scratch.workspace
--   local current_ws = hl.get_active_workspace()
--
--   if not current_ws then
--     return
--   end
--
--   if scratch.hidden or scratch_ws ~= current_ws then
--     hl.dispatch(hl.dsp.focus({ window = scratch, follow = false }))
--     hl.dispatch(hl.dsp.window.move({ workspace = current_ws.id }))
--   else
--     hl.dispatch(hl.dsp.window.move({ workspace = SCRATCH_WS, follow = false}))
--   end
-- end)
--
-- hl.bind(kbd({ MOD, LS, "X" }), function()
--   local window = hl.get_active_window()
--   if not window then
--     log("no window")
--     return
--   end
--
--   if not window.floating then
--     log("go tag")
--     hl.dispatch(hl.dsp.window.float({ action = "float" }))
--   end
--
--   hl.dispatch(hl.dsp.window.tag({ tag = SCRATCH }))
--
--     -- hl.notification.create({ text = "AAAA",  duration = 3000 })
--     -- hl.dsp.window.float({ action = "toggle" })
--     -- hl.dsp.exec_cmd("notify-send AAA")
--     -- local ws = hl.get_active_workspace()
--     -- print("active workspace: " .. tostring(ws))
--     -- print("active workspace layout: " .. tostring(ws.tiled_layout))
--     -- io.stderr:write("AAAAAAAAAAA")
--     -- io.stderr:flush()
--     log("done")
-- end)
