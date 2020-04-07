import random from 'lodash/random'
import React, {CSSProperties, ReactNode, useMemo} from 'react'
import styled from 'styled-components'

function stylizeText(text: string) {
  if (!text) {
    return ''
  }
  const transformedText = text.replace(/\bi\b/g, 'I')
  return transformedText
}

function flattenChildren(children: ReactNode) {
  const texts: Array<string> = []
  React.Children.forEach(children, c => {
    if (typeof c === 'string') {
      texts.push(c)
    } else if (typeof c === 'number') {
      texts.push(String(c))
    }
  })
  return texts.join('')
}

const Text: React.FC<{
  size?: number,
  className?: string,
  style?: CSSProperties,
}> = ({children, size, className, style}) => {
  const baseText = flattenChildren(children)
  const content = useMemo(() => {
    return stylizeText(baseText)
  }, [baseText])

  return (
    <SimpleText
      size={size}
      className={className}
      style={style}
    >
      {content}
    </SimpleText>
  )
}

interface SimpleTextProps {
  size?: number
}
export const SimpleText = styled.span<SimpleTextProps>`
  font-family: xkcd-Regular-v3;
  font-size: ${({size}) => size ? `${size}px`: null};
  line-height: ${({size}) => size ? `${size}px`: null};
  font-variant: normal;
`

export default Text
