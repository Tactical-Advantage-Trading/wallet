package trading.tacticaladvantage.utils;

import android.animation.ValueAnimator;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.View;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;

import trading.tacticaladvantage.R;

public class StripeBackground extends View {
    private final Paint basePaint = new Paint(Paint.ANTI_ALIAS_FLAG);
    private final Paint stripePaint = new Paint(Paint.ANTI_ALIAS_FLAG);
    private final Path clipPath = new Path();
    private final Path stripePath = new Path();
    private final RectF bounds = new RectF();

    private final float cornerRadius;
    private final float stripeWidth;
    private final float stripePeriod;

    private ValueAnimator animator;
    private boolean running;
    private float offset;

    public StripeBackground(Context context) {
        this(context, null);
    }

    public StripeBackground(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public StripeBackground(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        setWillNotDraw(false);
        cornerRadius = getResources().getDimension(R.dimen.corner_radius);
        stripeWidth = (float) 25.0 * getResources().getDisplayMetrics().density;
        float stripeGap = (float) 25.0 * getResources().getDisplayMetrics().density;
        stripePeriod = stripeWidth + stripeGap;
        basePaint.setColor(0x00000000);
        stripePaint.setColor(0x11AAAAAA);
        setVisibility(GONE);
    }

    public void setIdle(boolean shouldStop) {
        running = !shouldStop;
        setVisibility(shouldStop ? GONE : VISIBLE);

        if (running) startAnimator();
        else stopAnimator();
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        if (running) startAnimator();
    }

    @Override
    protected void onDetachedFromWindow() {
        stopAnimator();
        super.onDetachedFromWindow();
    }

    @Override
    protected void onDraw(@NonNull Canvas canvas) {
        super.onDraw(canvas);
        if (getWidth() == 0 || getHeight() == 0) return;

        bounds.set(0F, 0F, getWidth(), getHeight());
        clipPath.reset();
        clipPath.addRoundRect(bounds, cornerRadius, cornerRadius, Path.Direction.CW);

        int saveCount = canvas.save();
        canvas.clipPath(clipPath);
        canvas.drawRect(bounds, basePaint);

        float diagonalRun = getHeight();
        for (float x = -getHeight() - stripePeriod + offset; x < getWidth() + stripePeriod; x += stripePeriod) {
            stripePath.reset();
            stripePath.moveTo(x, getHeight());
            stripePath.lineTo(x + stripeWidth, getHeight());
            stripePath.lineTo(x + stripeWidth + diagonalRun, 0F);
            stripePath.lineTo(x + diagonalRun, 0F);
            stripePath.close();
            canvas.drawPath(stripePath, stripePaint);
        }

        canvas.restoreToCount(saveCount);
    }

    private void startAnimator() {
        if (!isAttachedToWindow()) return;
        if (animator == null) {
            animator = ValueAnimator.ofFloat(0F, stripePeriod);
            animator.setDuration(2000L);
            animator.setRepeatCount(ValueAnimator.INFINITE);
            animator.setInterpolator(new LinearInterpolator());
            animator.addUpdateListener(animation -> {
                offset = (Float) animation.getAnimatedValue();
                invalidate();
            });
        }

        if (!animator.isStarted()) animator.start();
    }

    private void stopAnimator() {
        if (animator != null) animator.cancel();
        offset = 0F;
        invalidate();
    }
}
