import React, {useEffect, useRef} from 'react'
import sample from 'lodash/sample'
import random from 'lodash/random'

import type {PreloadedArtImage} from '../preloadImg'
import {preloadImg} from '../preloadImg'
import useCanvas from '../useCanvas'

const particleImgs = [
  require('../../art/bubble-1.png'),
  require('../../art/bubble-2.png'),
  require('../../art/puff-1.png'),
  require('../../art/puff-2.png'),
  require('../../art/star-1.png'),
  require('../../art/star-2.png'),
  require('../../art/spiral.png'),
].map(preloadImg)

type ParticleState = {
  img: PreloadedArtImage,
  s: number,
  x: number,
  y: number,
  a: number,
  vx: number,
  vy: number,
  va: number,
  drag: number,
  time: number,
}

function* animate(
  canvas: HTMLCanvasElement,
  dpr: number,
  width: number,
  height: number,
): Generator<void> {
  const ctx = canvas.getContext('2d')
  ctx.scale(dpr, dpr)

  let particles: Array<ParticleState> = []
  for (let i = 0; i < random(60, 80); i++) {
    particles.push({
      img: sample(particleImgs),
      s: random(.4, 1.1),
      x: width * .9 + random(-12, 12),
      y: height * .75 + random(-8, 8),
      a: 0,
      vx: -width / 100 + random(0, 10.5),
      vy: -height / 60 + random(0, 8.5),
      va: random(-.1, .1),
      drag: random(-.02, 0),
      time: random(110, 125),
    })
  }

  while (particles.length) {
    ctx.clearRect(0, 0, width, height)
    for (const particle of particles) {
      particle.time--
      if (particle.time > 115) {
        continue
      }
      particle.vy += width / 5000
      particle.vx *= 1 + particle.drag
      particle.x += particle.vx
      particle.y += particle.vy
      particle.a += particle.va
      const {el, width: pw, height: ph} = particle.img
      ctx.save()
      ctx.translate(particle.x, particle.y)
      ctx.rotate(particle.a)
      ctx.scale(particle.s, particle.s)
      ctx.globalAlpha = particle.time > 60 ? 1 : particle.time / 60

      try {
        ctx.drawImage(el, -pw / 2, -ph / 2, pw, ph)
      } catch (err) {
        console.warn('Error drawing particle image', particle, err)
      }

      ctx.restore()
    }

    particles = particles.filter(p => p.time > 0)
    yield
  }
}

const ParticlePop: React.FC<{
  className?: string,
  width: number,
  height: number,
}> = ({className, width, height}) => {
  const [dpr, canvasWidth, canvasHeight, canvasRef] = useCanvas(width, height)

  useEffect(() => {
    const animation = animate(canvasRef.current, dpr, width, height)

    function step() {
      animation.next()
      requestAnimationFrame(step)
    }
    step()
  }, [])

  return (
    <canvas
      ref={canvasRef}
      className={className}
      width={canvasWidth}
      height={canvasHeight}
      style={{width, height}}
    />
  )
}

export default ParticlePop
