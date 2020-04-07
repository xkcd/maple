import React, {useCallback, useRef, useState} from 'react'
import styled, {css, keyframes} from 'styled-components'

import type {StoredInventoryItem} from '../types'
import lootImageURL from '../lootImageURL'
import {preloadImg} from '../preloadImg'
import ParticlePop from './ParticlePop'

const treasureClosedImg = preloadImg(require('../../art/treasure-closed.png'))
const treasureOpenImg = preloadImg(require('../../art/treasure-open.png'))

const LootButton: React.FC<{
  className?: string,
  firstNewLoot: StoredInventoryItem,
  openNewLoot: () => void,
}> = ({className, firstNewLoot, openNewLoot}) => {
  const [isOpen, setIsOpen] = useState(false)
  const handleOpenClick = useCallback(() => {
    setIsOpen(true)
    setTimeout(() => {
      setIsOpen(false)
      openNewLoot()
    }, 2000)
  }, [])

  return (
    <PositionContainer className={className}>
      <ShakeContainer canShake={!isOpen}>
        <StyledLootButton
          hasLoot={!!firstNewLoot}
          isOpen={isOpen}
          onClick={!!firstNewLoot && !isOpen ? handleOpenClick : null}
          tabIndex={0}
        >
          <img
            src={(isOpen ? treasureOpenImg : treasureClosedImg).url['2x']}
            width={(isOpen ? treasureOpenImg : treasureClosedImg).width}
            height={(isOpen ? treasureOpenImg : treasureClosedImg).height}
          />
        </StyledLootButton>
      </ShakeContainer>
      {isOpen && firstNewLoot && <NewLootItem
        style={{
          backgroundImage: `url(${lootImageURL(firstNewLoot.img)})`
        }}
      />}
      {isOpen && <PositionedParticlePop
        width={800}
        height={600}
      />}
    </PositionContainer>
  )
}

const PositionContainer = styled.div`
  position: relative;
`

const PositionedParticlePop = styled(ParticlePop)`
  position: absolute;
  right: 0;
  bottom: -100px;
  z-index: -1;
`

const shake = keyframes`
  0% {
    transform: rotate(-1deg);
  }

  50% {
    transform: rotate(1deg);
  }

  100% {
    transform: rotate(-1deg);
  }
`;

interface ShakeContainerProps {
  canShake: boolean,
}
const ShakeContainer = styled.div<ShakeContainerProps>`
  &:hover {
    animation: ${({canShake}) => canShake ? css`${shake} .35s linear infinite` : 'none'};
  }
`

interface StyledLootButtonProps {
  hasLoot: boolean,
  isOpen: boolean,
}
const StyledLootButton = styled.div<StyledLootButtonProps>`
  padding: 4px;
  border-bottom-width: 4px;
  margin-top: ${({isOpen}) => isOpen ? '0' : '27px'};
  margin-right: ${({isOpen}) => isOpen ? '0' : '27px'};
  transition: transform .35s ease;
  transform: ${({hasLoot}) => hasLoot ? 'none' : 'scale(0)'};
  cursor: pointer;

  &:focus {
    outline: none;
  }

  ${({isOpen}) => isOpen ? css`
    cursor: default;
  ` : css`
    &:hover {
      transform: scale(1.1);
      transition-duration: .15s;
    }

    &:active {
      transform: scale(1.05);
      transition-duration: .15s;
    }
  `}

`

const slideUp = keyframes`
  0% {
    opacity: 0;
    transform: translateY(70px) scale(.25);
  }

  100% {
    opacity: 1;
    transform: translateY(0) scale(1);
  }
`;

const NewLootItem = styled.div`
  position: absolute;
  right: 50px;
  bottom: 110px;
  width: 100px;
  height: 100px;
  padding: 5px;
  background: white center center no-repeat;
  background-size: contain;
  animation: ${slideUp} .75s ease;
  border: 2px solid black;
`

export default LootButton
