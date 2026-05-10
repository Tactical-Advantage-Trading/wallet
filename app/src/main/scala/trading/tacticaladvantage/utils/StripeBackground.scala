package trading.tacticaladvantage.utils

import android.animation.ValueAnimator
import android.content.Context
import android.graphics.{Canvas, Paint, Path, RectF}
import android.util.AttributeSet
import android.view.View
import android.view.animation.LinearInterpolator
import trading.tacticaladvantage.R


class StripeBackground(context: Context, attrs: AttributeSet, defStyleAttr: Int ) extends View(context, attrs, defStyleAttr) {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null, 0)

  private val stripePaint = new Paint(Paint.ANTI_ALIAS_FLAG)
  private val basePaint = new Paint(Paint.ANTI_ALIAS_FLAG)
  private val stripePath = new Path
  private val clipPath = new Path
  private val bounds = new RectF

  private val stripeWidth = 25f * getResources.getDisplayMetrics.density
  private val stripePeriod = stripeWidth + 25f * getResources.getDisplayMetrics.density
  private val cornerRadius = getResources.getDimension(R.dimen.corner_radius)
  private var animator: ValueAnimator = _
  private var offset = 0f
  private var on = false

  setWillNotDraw(false)
  basePaint.setColor(0x00000000)
  stripePaint.setColor(0x11AAAAAA)
  setVisibility(View.GONE)

  def setIdle(shouldStop: Boolean): Unit = {
    setVisibility { if (shouldStop) View.GONE else View.VISIBLE }
    if (shouldStop) stopAnimator else startAnimator
    on = !shouldStop
  }

  override protected def onAttachedToWindow: Unit = {
    super.onAttachedToWindow
    if (on) startAnimator
  }

  override protected def onDetachedFromWindow: Unit = {
    stopAnimator
    super.onDetachedFromWindow
  }

  override protected def onDraw(canvas: Canvas): Unit = {
    super.onDraw(canvas)

    if (getWidth == 0 || getHeight == 0) return

    val w = getWidth.toFloat
    val h = getHeight.toFloat

    bounds.set(0f, 0f, w, h)

    clipPath.reset
    clipPath.addRoundRect(bounds, cornerRadius, cornerRadius, Path.Direction.CW)

    val saveCount = canvas.save

    canvas.clipPath(clipPath)
    canvas.drawRect(bounds, basePaint)
    var x = -h - stripePeriod + offset

    while (x < w + stripePeriod) {
      stripePath.reset
      stripePath.moveTo(x, h)
      stripePath.lineTo(x + stripeWidth, h)
      stripePath.lineTo(x + stripeWidth + h, 0f)
      stripePath.lineTo(x + h, 0f)
      stripePath.close

      canvas.drawPath(stripePath, stripePaint)
      x += stripePeriod
    }

    canvas.restoreToCount(saveCount)
  }

  private def startAnimator: Unit = {
    if (!isAttachedToWindow) return

    if (animator == null) {
      animator = ValueAnimator.ofFloat(0f, stripePeriod)
      animator.setRepeatCount(ValueAnimator.INFINITE)
      animator.setInterpolator(new LinearInterpolator)
      animator.setDuration(2000L)

      animator addUpdateListener new ValueAnimator.AnimatorUpdateListener {
        override def onAnimationUpdate(animation: ValueAnimator): Unit = {
          offset = animation.getAnimatedValue.asInstanceOf[Float]
          invalidate
        }
      }
    }

    if (!animator.isStarted) animator.start
  }

  private def stopAnimator: Unit = {
    if (animator != null) animator.cancel
    offset = 0f
    invalidate
  }
}