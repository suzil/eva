import type { Config } from 'tailwindcss'

const config: Config = {
  content: ['./index.html', './src/**/*.{ts,tsx}'],
  theme: {
    extend: {
      colors: {
        terminal: {
          950: '#06060C',
          900: '#0A0B12',
          850: '#0E0F18',
          800: '#13141F',
          750: '#191A28',
          700: '#212233',
          600: '#2C2D42',
          500: '#3A3B54',
          400: '#4F5070',
          300: '#6E6F8D',
          200: '#9596AF',
          100: '#C4C5D6',
          50:  '#EAEAF2',
        },
        'at-field': {
          950: '#261000',
          900: '#4D2000',
          800: '#803600',
          700: '#B34B00',
          600: '#E05E00',
          500: '#FF6A00',
          400: '#FF8533',
          300: '#FFA366',
          200: '#FFC299',
          100: '#FFE0CC',
          50:  '#FFF4EB',
        },
        'eva-green': {
          950: '#001F09',
          900: '#003D13',
          800: '#00661F',
          700: '#008A2B',
          600: '#00B338',
          500: '#00DD44',
          400: '#33FF5C',
          300: '#66FF85',
          200: '#A3FFB8',
          100: '#D0FFDB',
          50:  '#EDFFF0',
        },
        'nerv-red': {
          950: '#2B0202',
          900: '#550505',
          800: '#880808',
          700: '#B30D0D',
          600: '#DD1111',
          500: '#FF2222',
          400: '#FF4444',
          300: '#FF7A7A',
          200: '#FFB0B0',
          100: '#FFD6D6',
          50:  '#FFF0F0',
        },
        'magi-blue': {
          950: '#001A2B',
          900: '#003355',
          800: '#004D80',
          700: '#0066AA',
          600: '#0088DD',
          500: '#00AAFF',
          400: '#33BBFF',
          300: '#66CCFF',
          200: '#99DDFF',
          100: '#CCF0FF',
          50:  '#EBF8FF',
        },
        'warn-amber': {
          950: '#2B1F00',
          900: '#553D00',
          800: '#805B00',
          700: '#AA7A00',
          600: '#DD9E00',
          500: '#FFBB00',
          400: '#FFCC33',
          300: '#FFD966',
          200: '#FFE699',
          100: '#FFF3CC',
          50:  '#FFFBEB',
        },
        // Node type accents — single values, used in Tailwind utilities
        // and as raw hex in React Flow inline styles
        'node-agent':     '#7B4AE2',
        'node-trigger':   '#FF3B3B',
        'node-knowledge': '#00BBFF',
        'node-action':    '#00DD44',
        'node-connector': '#FF8800',
      },

      fontFamily: {
        sans:    ['Inter', 'system-ui', 'sans-serif'],
        mono:    ['"JetBrains Mono"', '"Fira Code"', 'monospace'],
        display: ['Rajdhani', 'Inter', 'sans-serif'],
      },

      fontSize: {
        'xxs':  ['10px', { lineHeight: '14px' }],
        'xs':   ['11px', { lineHeight: '16px' }],
        'sm':   ['12px', { lineHeight: '18px' }],
        'base': ['13px', { lineHeight: '20px' }],
        'lg':   ['14px', { lineHeight: '22px' }],
        'xl':   ['16px', { lineHeight: '24px' }],
        '2xl':  ['20px', { lineHeight: '28px' }],
        '3xl':  ['24px', { lineHeight: '32px' }],
      },

      keyframes: {
        // Running node ring pulse
        'glow-pulse': {
          '0%, 100%': { opacity: '1', boxShadow: '0 0 6px 0 currentColor' },
          '50%':       { opacity: '0.6', boxShadow: '0 0 12px 2px currentColor' },
        },
        // Edge flow animation (stroke-dashoffset)
        'dash-flow': {
          '0%':   { strokeDashoffset: '24' },
          '100%': { strokeDashoffset: '0' },
        },
        // Step complete — brief scale bump
        'step-done': {
          '0%':   { transform: 'scale(1)' },
          '40%':  { transform: 'scale(1.08)' },
          '100%': { transform: 'scale(1)' },
        },
        // Step failed — horizontal shake
        'step-fail': {
          '0%, 100%': { transform: 'translateX(0)' },
          '20%':      { transform: 'translateX(-3px)' },
          '40%':      { transform: 'translateX(3px)' },
          '60%':      { transform: 'translateX(-2px)' },
          '80%':      { transform: 'translateX(2px)' },
        },
        // Shimmer for skeleton loaders
        shimmer: {
          '0%':   { backgroundPosition: '-200% 0' },
          '100%': { backgroundPosition: '200% 0' },
        },
        // Boot sequence text appearance (used in BootScreen)
        'type-in': {
          '0%':   { opacity: '0', transform: 'translateY(4px)' },
          '100%': { opacity: '1', transform: 'translateY(0)' },
        },
      },

      animation: {
        'glow-pulse': 'glow-pulse 1.5s ease-in-out infinite',
        'dash-flow':  'dash-flow 1s linear infinite',
        'step-done':  'step-done 200ms ease-out',
        'step-fail':  'step-fail 300ms ease-in-out',
        shimmer:      'shimmer 1.5s ease-in-out infinite',
        'type-in':    'type-in 150ms ease-out both',
      },

      // Transition speeds
      transitionDuration: {
        snap:   '75ms',
        fast:   '150ms',
        normal: '250ms',
        slow:   '400ms',
      },

      // Box shadow presets
      boxShadow: {
        'node':          '0 4px 16px rgba(0, 0, 0, 0.5)',
        'node-agent':    '0 0 12px rgba(123, 74, 226, 0.15)',
        'node-trigger':  '0 0 12px rgba(255, 59, 59, 0.15)',
        'node-knowledge':'0 0 12px rgba(0, 187, 255, 0.15)',
        'node-action':   '0 0 12px rgba(0, 221, 68, 0.15)',
        'node-connector':'0 0 12px rgba(255, 136, 0, 0.15)',
        'modal':         '0 25px 50px rgba(0, 0, 0, 0.8)',
      },
    },
  },
  plugins: [],
}

export default config
