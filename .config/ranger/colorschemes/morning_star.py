from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *

class Default(ColorScheme):
    def use(self, context):
        #fg   = 0
        #attr = default_colors[2]
        #bg   = -1

        fg, bg, attr = default_colors
        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.fifo or context.socket:
                fg = 2
            if context.device:
                bg = 77
            if context.badinfo:
                fg = 64
            if context.error:
                fg = 64
            if context.container:
                fg = 13
            if context.border:
                fg = 7
            if context.executable:
                fg = 9
            if context.empty:
                fg = 81
            if context.directory:
                attr = bold
                fg = 12
            if context.selected:
                attr = reverse
            elif context.device:
                fg = 12
            if context.media:
                fg = 10
                
        elif context.in_titlebar:
            attr = bold
            if context.hostname:
                fg = context.bad and red or green
            elif context.directory:
                fg = 13
            elif context.tab:
                if context.good:
                    fg = 3
            elif context.link:
                fg = 14

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = 10
                elif context.bad:
                    fg = 9
            if context.marked:
                attr = bold
                fg = 3
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = 9
            if context.vcsinfo:
                fg = 12
                attr = bold
            if context.vcscommit:
                fg = 11
                attr = bold
        
        return fg, bg, attr

